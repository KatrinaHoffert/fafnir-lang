case class Block(statements: List[Statement]) extends Statement {
  override def execute(state: ProgramState): Unit = {
    state.variables.enterScope()
    for(statement <- statements) {
      statement.execute(state)
    }
    state.variables.exitScope()
  }

  override def toString: String = "{\n" + statements.map(Constants.indentation + _.toString).mkString("\n") + "\n}"
}

case class AssignmentStatement(declaration: Boolean, identifier: Identifier, expression: Expression) extends Statement {
  override def execute(state: ProgramState): Unit = {
    if(!state.variables.contains(identifier.name) && !declaration) {
      throw new Exception(s"Assignment to undeclared variable $identifier")
    }
    state.variables(identifier.name) = expression.evaluate(state)
  }

  override def toString: String = s"${if(declaration) "var " else ""}$identifier = $expression;"
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

  override def toString: String = s"if($expression) $ifBlock"
}

abstract class Statement {
  def execute(state: ProgramState): Unit
}
