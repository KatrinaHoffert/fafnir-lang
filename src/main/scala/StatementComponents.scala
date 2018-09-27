case class AssignmentStatement(identifier: Identifier, expression: Expression) extends Statement {
  override def execute(state: ProgramState): Unit = {
    state.globals(identifier.name) = expression.evaluate
  }
  override def toString: String = s"$identifier = $expression"
}

abstract class Statement {
  def execute(state: ProgramState): Unit
}

class ProgramState() {
  val globals: collection.mutable.Map[String, ValueInstance] = collection.mutable.Map()
}