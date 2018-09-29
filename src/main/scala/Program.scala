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
  /**
    * Scope used for global variables.
    */
  private val globalScopeStack = collection.mutable.ListBuffer(collection.mutable.Map[String, ValueInstance]())

  /**
    * Frames for functions. Each frame has its own scope stack.
    */
  private val frameAndScopeStack = collection.mutable.ListBuffer[collection.mutable.ListBuffer[collection.mutable.Map[String, ValueInstance]]]()

  def apply(name: String): ValueInstance = {
    frameAndScopeStack.headOption.getOrElse(List()).find(_.contains(name)) match {
      case Some(scope) =>
        scope(name)
      case None =>
        // Not found locally, so search the global scope
        globalScopeStack.find(_.contains(name)) match {
          case Some(scope) => scope(name)
          case None => throw new Exception(s"Variable $name is not defined")
        }
    }
  }

  def update(name: String, value: ValueInstance): Unit = {
    frameAndScopeStack.headOption.getOrElse(List()).find(_.contains(name)) match {
      case Some(scope) =>
        scope(name) = value
      case None =>
        // Not found locally, so search the global scope
        globalScopeStack.find(_.contains(name)) match {
          case Some(scope) => scope(name) = value
          case None =>
            // New variable. If we're in a frame, set it in the newest frame scope. Otherwise the newest global scope.
            if(frameAndScopeStack.nonEmpty) {
              frameAndScopeStack.head.head(name) = value
            }
            else {
              globalScopeStack.head(name) = value
            }
        }
    }
  }

  /***
    * Sets a variable on the latest frame scope, allowing for ghosting over other frames or globals (necessary for
    * parameters, for one thing).
    */
  def setFrameVariable(name: String, value: ValueInstance): Unit = frameAndScopeStack.head.head(name) = value

  /**
    * Checks if a variable exists in any accessible scope.
    */
  def contains(name: String): Boolean = {
    frameAndScopeStack.headOption.getOrElse(List()).exists(_.contains(name)) || globalScopeStack.exists(_.contains(name))
  }

  /**
    * Creates a new frame and a scope within that.
    */
  def enterFrame(): Unit = {
    frameAndScopeStack.prepend(collection.mutable.ListBuffer())
    frameAndScopeStack.head.prepend(collection.mutable.Map[String, ValueInstance]())
  }

  /**
    * Creates a new scope level. If we're in a frame, we'll create the scope there. Otherwise it's created globally.
    */
  def enterScope(): Unit = {
    frameAndScopeStack.headOption match {
      case Some(frame) => frame.prepend(collection.mutable.Map[String, ValueInstance]())
      case None => globalScopeStack.prepend(collection.mutable.Map[String, ValueInstance]())
    }
  }

  /**
    * Exits the current scope, removing access to variables that only exist in it. If we're in a frame, we remove the
    * scope from that frame. If we removed the last scope in the frame, we'll exit the frame. If we're not in a frame,
    * we exit a global scope. It's assumed that entering and exiting scopes is always balanced.
    */
  def exitScope(): Unit = {
    frameAndScopeStack.headOption match {
      case Some(frame) =>
        frame.remove(0)
        if(frame.isEmpty) frameAndScopeStack.remove(0)
      case None => globalScopeStack.remove(0)
    }
  }

  /**
    * @return A new map containing *all* variables as they are understood to be at this point of time. Note that frame
    *         variables can ghost globals (but the globals are still there).
    */
  def allVariables: Map[String, ValueInstance] = {
    val variables = collection.mutable.Map[String, ValueInstance]()
    for(scope <- globalScopeStack.reverseIterator) {
      variables ++= scope
    }
    for(scope <- frameAndScopeStack.headOption.getOrElse(List()).reverseIterator) {
      variables ++= scope
    }

    variables.toMap
  }
}