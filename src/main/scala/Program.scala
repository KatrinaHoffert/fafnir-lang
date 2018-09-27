case class Program(statements: List[Statement]) {
  def execute(): ProgramState = {
    val state = new ProgramState()

    for(statement <- statements) {
      statement.execute(state)
    }

    state
  }

  override def toString: String = statements.map(_.toString).mkString("\n")
}

class ProgramState() {
  val globals: collection.mutable.Map[String, ValueInstance] = collection.mutable.Map()
}