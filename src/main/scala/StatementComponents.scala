import scala.util.parsing.input.Positional

case class Block(statements: List[Statement]) extends Statement {
  override def execute(state: ProgramState): Unit = {
    state.variables.enterScope()
    for(statement <- statements) {
      statement.execute(state)
    }
    state.variables.exitScope()
  }

  override def toString: String = {
    // Indent all lines inside the block
    val statementLines = statements.map(_.toString).mkString("\n")
    val indentedStatementLines = statementLines.split('\n').map(line => Constants.indentation + line).mkString("\n")

    s"{\n$indentedStatementLines\n}"
  }
}

case class AssignmentStatement(declaration: Boolean, identifier: Identifier, expression: Expression) extends Statement {
  override def execute(state: ProgramState): Unit = {
    if(!state.variables.contains(identifier.name) && !declaration) {
      throw new FafnirRuntimeException(identifier, s"Assignment to undeclared variable $identifier")
    }
    state.variables(identifier.name) = expression.evaluate(state)
  }

  override def toString: String = s"${if(declaration) "var " else ""}$identifier = $expression;"
}

case class FunctionDeclaration(identifier: Identifier, args: List[Identifier], body: Block) extends Statement {
  override def execute(state: ProgramState): Unit = {
    if(state.variables.contains(identifier.name)) {
      throw new FafnirRuntimeException(identifier, s"Cannot assign new function to existing variable $identifier")
    }

    state.variables(identifier.name) = FunctionValue(args, body.statements)
  }

  override def toString: String = s"func $identifier(${args.mkString(", ")}) $body"
}

case class FunctionCallStatement(function: FunctionCall) extends Statement {
  override def execute(state: ProgramState): Unit = {
    // Only has an effect if there's side effects
    function.evaluate(state)
  }

  override def toString: String = s"$function;"
}

case class ReturnStatement(expression: Option[Expression]) extends Statement {
  override def execute(state: ProgramState): Unit = {
    if(!state.variables.inFrame) {
      throw new FafnirRuntimeException(this, "Cannot return when not in a function.")
    }

    val returnValue = expression.map(_.evaluate(state))
    state.signalReturning(returnValue.getOrElse(VoidValue()))
  }

  override def toString: String = s"return $expression;"
}

case class IfStatement(expression: Expression, ifBlock: Block, elifSections: List[IfStatement], elseSection: Option[Block]) extends Statement {
  override def execute(state: ProgramState): Unit = {
    val expressionResult = expression.evaluate(state)
    if(expressionResult.isTruthy) {
      ifBlock.execute(state)
    }
    else {
      // One by one, evaluate each elif and execute only the first one that is true (at which point we should NOT
      // evaluate any more expressions because they may have side effects.
      var wentIntoElif = false
      for(section <- elifSections if !wentIntoElif) {
        val elifExpressionResult = section.expression.evaluate(state)
        if(elifExpressionResult.isTruthy) {
          section.ifBlock.execute(state)
          wentIntoElif = true
        }
      }

      if(!wentIntoElif) {
        elseSection.foreach(_.execute(state))
      }
    }
  }

  override def toString: String = {
    val elifSectionsString = if(elifSections.nonEmpty) {
      "\n" + elifSections.map(section => s"elif(${section.expression}) ${section.ifBlock}").mkString("\n")
    }
    else {
      ""
    }
    val elseSectionString = if(elseSection.isDefined) {
      "\n" + s"else ${elseSection.get}"
    }
    else {
      ""
    }

    s"if($expression) $ifBlock" + elifSectionsString + elseSectionString
  }
}

case class WhileLoop(expression: Expression, whileBlock: Block) extends Statement {
  override def execute(state: ProgramState): Unit = {
    while(expression.evaluate(state).isTruthy) {
      whileBlock.execute(state)
    }
  }

  override def toString: String = s"while($expression) $whileBlock"
}

abstract class Statement extends Positional {
  def execute(state: ProgramState): Unit
}
