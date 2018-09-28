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

case class IfStatement(expression: Expression, ifBlock: Block) extends Statement {
  override def execute(state: ProgramState): Unit = {
    val expressionResult = expression.evaluate(state)
    println(s"Expression: $expression evaluated to $expressionResult and truthiness is ${expressionResult.isTruthy}")
    if(expressionResult.isTruthy) {
      ifBlock.execute(state)
    }
  }

  override def toString: String = s"if($expression) $ifBlock"
}

abstract class Statement {
  def execute(state: ProgramState): Unit
}
