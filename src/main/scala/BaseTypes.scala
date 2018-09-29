abstract class ValueInstance {
  def +(that: ValueInstance): ValueInstance = throw new Exception("Not supported type")
  def -(that: ValueInstance): ValueInstance = throw new Exception("Not supported type")
  def *(that: ValueInstance): ValueInstance = throw new Exception("Not supported type")
  def /(that: ValueInstance): ValueInstance = throw new Exception("Not supported type")
  def call(state: ProgramState, argValues: List[ValueInstance]): ValueInstance = throw new Exception("Not supported type")
  def isTruthy: Boolean = true
}

case class IntValue(value: Int) extends ValueInstance {
  override def +(that: ValueInstance): ValueInstance = {
    that match {
      case intThat: IntValue => IntValue(value + intThat.value)
      case _ => throw new Exception("Not supported type")
    }
  }

  override def -(that: ValueInstance): ValueInstance = {
    that match {
      case intThat: IntValue => IntValue(value - intThat.value)
      case _ => throw new Exception("Not supported type")
    }
  }

  override def *(that: ValueInstance): ValueInstance = {
    that match {
      case intThat: IntValue => IntValue(value * intThat.value)
      case _ => throw new Exception("Not supported type")
    }
  }

  override def /(that: ValueInstance): ValueInstance = {
    that match {
      case intThat: IntValue => IntValue(value / intThat.value)
      case _ => throw new Exception("Not supported type")
    }
  }

  override def isTruthy: Boolean = value != 0

  override def toString: String = value.toString
}

case class StringValue(value: String) extends ValueInstance {
  override def +(that: ValueInstance): ValueInstance = {
    that match {
      case stringThat: StringValue => StringValue(value + stringThat.value)
      case _ => throw new Exception("Not supported type")
    }
  }

  override def *(that: ValueInstance): ValueInstance = {
    that match {
      case intThat: IntValue => StringValue(value * intThat.value)
      case _ => throw new Exception("Not supported type")
    }
  }

  override def isTruthy: Boolean = value.length > 0

  override def toString: String = s""""$value""""
}

case class VoidValue() extends ValueInstance {
  override def isTruthy: Boolean = false
  override def toString: String = "Void"
}

case class FunctionValue(args: List[Identifier], body: List[Statement]) extends ValueInstance {
  override def call(state: ProgramState, argValues: List[ValueInstance]): ValueInstance = {
    if(args.length != argValues.length) {
      throw new Exception(s"Function takes ${args.length} arguments but only ${argValues.length} were provided.")
    }

    state.variables.enterFrame()
    for((variableName, variableValue) <- args.zip(argValues)) {
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

  override def toString: String = "Function"
}