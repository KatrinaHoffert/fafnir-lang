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
  val variables: Scopes = new Scopes()
}

/**
  * Represents a stack of scopes with simple ways to update variables and search within the scopes. Follows
  * straightforward rules, with the most recent scope of a variable being updated.
  */
class Scopes() {
  private val scopeStack: collection.mutable.ListBuffer[collection.mutable.Map[String, ValueInstance]] = collection.mutable.ListBuffer(collection.mutable.Map[String, ValueInstance]())

  def apply(name: String): ValueInstance = {
    scopeStack.find(_.contains(name)) match {
      case Some(scope) => scope(name)
      case None => throw new Exception(s"Variable $name is not defined")
    }
  }

  def update(name: String, value: ValueInstance): Unit = {
    scopeStack.find(_.contains(name)) match {
      case Some(scope) => scope(name) = value
      case None => scopeStack.head(name) = value // Set it in the newest scope
    }
  }

  /**
    * Pushes a variable onto the newest scope in the stack, ignoring if it might already be in an earlier scope (thus
    * allowing ghosting).
    */
  def pushVariableOntoStack(name: String, value: ValueInstance): Unit = {
    scopeStack.head(name) = value
  }

  def contains(name: String): Boolean = scopeStack.exists(_.contains(name))

  def enterScope(): Unit = scopeStack.prepend(collection.mutable.Map[String, ValueInstance]())

  def exitScope(): Unit = scopeStack.remove(0)

  def allVariables: Map[String, ValueInstance] = {
    val variables = collection.mutable.Map[String, ValueInstance]()
    for(scope <- scopeStack.reverseIterator) {
      variables ++= scope
    }
    variables.toMap
  }
}