import scala.util.parsing.input.Positional

case class Block(statements: List[Statement]) extends Statement {
  override def execute(state: ProgramState): Unit = {
    state.variables.enterScope()
    for(statement <- statements) {
      statement.execute(state)
    }
    state.variables.exitScope()
  }

  override def staticCheck(staticInfo: StaticInfo): Unit = {
    statements.foreach(_.staticCheck(staticInfo))
  }

  override def toString: String = {
    // Indent all lines inside the block
    val statementLines = statements.map(_.toString).mkString("\n")
    val indentedStatementLines = statementLines.split('\n').map(line => Constants.indentation + line).mkString("\n")

    s"{\n$indentedStatementLines\n}"
  }
}
case class AssignmentStatement(declaringType: Option[Identifier], identifier: Identifier, expression: Expression) extends Statement {
  override def execute(state: ProgramState): Unit = {
    state.variables(identifier.name) = expression.evaluate(state)
  }

  override def staticCheck(staticInfo: StaticInfo): Unit = {
    val (fullyQualifiedName, existingVariableType) = staticInfo.getFullyQualifiedVariableNameAndType(identifier.name)

    // Prevent dumb programmer errors
    if(existingVariableType.isEmpty && declaringType.isEmpty) {
      throw new FafnirRuntimeException(identifier, s"Assignment to undeclared variable $identifier")
    }
    else if(existingVariableType.isDefined && declaringType.isDefined) {
      throw new FafnirRuntimeException(identifier, s"Duplicate assignment to variable $identifier")
    }

    // Verify the expression is a compatible type for the assignment
    val valueType = expression.evaluateType(staticInfo)
    declaringType match {
      case Some(variableType) =>
        if (variableType.name != valueType) {
          throw new FafnirRuntimeException(identifier, s"Type mismatch on $identifier. Expected ${variableType.name} " +
            s"but got $valueType.")
        }

        // New declaration, so save the type
        staticInfo.variableTypes(fullyQualifiedName) = variableType.name
      case None =>
        if (existingVariableType.get != valueType) {
          throw new FafnirRuntimeException(identifier, s"Type mismatch on $identifier. Expected " +
            s"${existingVariableType.get} but got $valueType.")
        }
    }
  }

  override def toString: String = {
    val leadingVar = if(declaringType.isDefined) "var " else ""
    val typeDefinition = if(declaringType.isDefined) s": ${declaringType.get}" else ""
    s"$leadingVar$identifier$typeDefinition = $expression;"
  }
}

case class FunctionDeclaration(identifier: Identifier, parameters: List[(Identifier, Identifier)],
                               returnType: Identifier, body: Block) extends Statement {
  override def execute(state: ProgramState): Unit = {
    if(state.variables.contains(identifier.name)) {
      throw new FafnirRuntimeException(identifier, s"Cannot assign new function to existing variable $identifier")
    }

    state.variables(identifier.name) = FunctionValue(identifier, parameters.map(_._1), body.statements)
  }

  override def staticCheck(staticInfo: StaticInfo): Unit = {
    val (fullyQualifiedName, existingVariableType) = staticInfo.getFullyQualifiedVariableNameAndType(identifier.name)
    if(existingVariableType.isDefined) {
      throw new FafnirRuntimeException(identifier, s"Cannot assign new function to existing variable $identifier")
    }
    staticInfo.variableTypes(fullyQualifiedName) = "Function"

    for((parameter, parameterType) <- parameters) {
      staticInfo.variableTypes(s"$fullyQualifiedName$$${parameter.name}") = parameterType.name
    }

    // Function should be mangled like `functionName__ParamType1__ParamType2`
    val mangledFunctionName = (fullyQualifiedName +: parameters.map(_._2)).mkString("__")
    staticInfo.functionSignatures(mangledFunctionName) = (returnType.name, parameters.map{ case (param, paramType) => (param.name, paramType.name)})

    // Body of the function needs to be checked now
    staticInfo.currentLocation.append(identifier.name)
    body.staticCheck(staticInfo)
    staticInfo.currentLocation.remove(staticInfo.currentLocation.length - 1)
  }

  override def toString: String = {
    val parameterString = parameters.map{ case (param, paramType) => s"$param: $paramType" }.mkString(", ")
    s"func $identifier($parameterString): $returnType $body"
  }
}

case class FunctionCallStatement(function: FunctionCall) extends Statement {
  override def execute(state: ProgramState): Unit = {
    // Only has an effect if there's side effects
    function.evaluate(state)
  }

  override def staticCheck(staticInfo: StaticInfo): Unit = {
    // Calls to nested defined functions not supported (how to get FQN?)
    val argumentTypes = function.argExpressions.map(_.evaluateType(staticInfo))
    val mangledName = (function.identifier.name +: argumentTypes).mkString("__")

    // Lots of room for improvement with reporting here
    if(!staticInfo.functionSignatures.contains(mangledName)) {
      val parametersString = "(" + argumentTypes.mkString(", ") + ")"
      throw new FafnirRuntimeException(function.identifier, s"Function ${function.identifier}$parametersString does not exist")
    }
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

  override def staticCheck(staticInfo: StaticInfo): Unit = {
    // Just find any incompatible type interactions
    expression.foreach(_.evaluateType(staticInfo))
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

  override def staticCheck(staticInfo: StaticInfo): Unit = {
    // Find any incompatible type interactions
    expression.evaluateType(staticInfo)
    elifSections.foreach(_.expression.evaluateType(staticInfo))

    ifBlock.staticCheck(staticInfo)
    elifSections.foreach(_.ifBlock.staticCheck(staticInfo))
    elseSection.foreach(_.staticCheck(staticInfo))
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

  override def staticCheck(staticInfo: StaticInfo): Unit = {
    // Find any incompatible type interactions
    expression.evaluateType(staticInfo)

    whileBlock.staticCheck(staticInfo)
  }

  override def toString: String = s"while($expression) $whileBlock"
}

abstract class Statement extends Positional {
  def execute(state: ProgramState): Unit
  def staticCheck(staticInfo: StaticInfo): Unit
}
