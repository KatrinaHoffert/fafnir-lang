case class Block(statements: List[Statement]) extends Statement {
  override def execute(state: ProgramState): Unit = {
    for(statement <- statements) {
      statement.execute(state)
    }
  }

  override def toString: String = "{\n" + statements.map(Constants.indentation + _.toString).mkString("\n") + "\n}"
}

case class AssignmentStatement(declaration: Boolean, identifier: Identifier, expression: Expression) extends Statement {
  override def execute(state: ProgramState): Unit = {
    if(!state.globals.contains(identifier.name) && !declaration) {
      throw new Exception(s"Assignment to undeclared variable $identifier")
    }
    state.globals(identifier.name) = expression.evaluate(state)
  }

  override def toString: String = s"${if(declaration) "var " else ""}$identifier = $expression;"
}

abstract class Statement {
  def execute(state: ProgramState): Unit
}
