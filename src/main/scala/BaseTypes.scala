/**
  * Represents an instance of a value (ie, of a variable or literal). Values have a type and various forms of
  * behavior.
  */
abstract class ValueInstance {
  val typeName: String

  def +(that: ValueInstance): ValueInstance = unsupportedOperation("+", that)
  def -(that: ValueInstance): ValueInstance = unsupportedOperation("-", that)
  def *(that: ValueInstance): ValueInstance = unsupportedOperation("*", that)
  def /(that: ValueInstance): ValueInstance = unsupportedOperation("/", that)
  def call(state: ProgramState, arguments: List[ValueInstance]): ValueInstance = {
    throw new FafnirOperationException(s"Type $typeName is not callable")
  }

  /**
    * @return Whether or not the variable should be considered truthy for the purposes of conditionals.
    */
  def isTruthy: Boolean = true

  protected def unsupportedOperation(op: String, that: ValueInstance): Nothing = {
    throw new FafnirOperationException(s"Operation $op is not defined on types $typeName and ${that.typeName}")
  }
}

case class IntValue(value: Int) extends ValueInstance {
  override val typeName: String = "Int"

  override def +(that: ValueInstance): ValueInstance = {
    that match {
      case intThat: IntValue => IntValue(value + intThat.value)
      case _ => unsupportedOperation("+", that)
    }
  }

  override def -(that: ValueInstance): ValueInstance = {
    that match {
      case intThat: IntValue => IntValue(value - intThat.value)
      case _ => unsupportedOperation("-", that)
    }
  }

  override def *(that: ValueInstance): ValueInstance = {
    that match {
      case intThat: IntValue => IntValue(value * intThat.value)
      case stringThat: StringValue => StringValue(stringThat.value * value)
      case _ => unsupportedOperation("*", that)
    }
  }

  override def /(that: ValueInstance): ValueInstance = {
    that match {
      case intThat: IntValue => {
        if(intThat.value == 0) throw new FafnirOperationException("Division by zero")
        IntValue(value / intThat.value)
      }
      case _ => unsupportedOperation("/", that)
    }
  }

  override def isTruthy: Boolean = value != 0

  override def toString: String = value.toString
}

case class StringValue(value: String) extends ValueInstance {
  override val typeName: String = "String"

  override def +(that: ValueInstance): ValueInstance = {
    that match {
      case stringThat: StringValue => StringValue(value + stringThat.value)
      case _ => unsupportedOperation("+", that)
    }
  }

  override def *(that: ValueInstance): ValueInstance = {
    that match {
      case intThat: IntValue => StringValue(value * intThat.value)
      case _ => unsupportedOperation("*", that)
    }
  }

  override def isTruthy: Boolean = value.length > 0

  override def toString: String = s""""$value""""
}

/**
  * The Void type is just used so that we can correctly handle someone trying to do an operation on the result of a
  * function that doesn't return anything (technically, it returns this).
  */
case class VoidValue() extends ValueInstance {
  override val typeName: String = "Void"
  override def isTruthy: Boolean = false
  override def toString: String = "Void"
}

case class FunctionValue(identifier: Identifier, parameters: List[Identifier], body: List[Statement]) extends ValueInstance {
  override val typeName: String = "Function"

  override def call(state: ProgramState, arguments: List[ValueInstance]): ValueInstance = {
    if(parameters.length != arguments.length) {
      throw new FafnirOperationException(s"Function ${identifier.name} takes ${parameters.length} arguments but " +
        s"${arguments.length} were provided")
    }

    // Deal with infinite recursion here
    try {
      state.variables.enterFrame()
    }
    catch {
      case ex: IllegalStateException => throw new FafnirOperationException(s"Max recursion depth exceeded in function ${identifier.name}")
    }

    for((variableName, variableValue) <- parameters.zip(arguments)) {
      state.variables.setFrameVariable(variableName.name, variableValue)
    }

    for(statement <- body if !state.isReturning) {
      statement.execute(state)
    }
    state.variables.exitScope()

    // Never hit a return statement? Function returned Void.
    if(!state.isReturning) state.returnValue = VoidValue()

    // Get the return value
    state.isReturning = false
    state.returnValue
  }

  override def toString: String = s"Function ${identifier.name}"
}