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

case class FunctionValue(args: List[Identifier], body: List[Statement]) extends ValueInstance {
  override def call(state: ProgramState, argValues: List[ValueInstance]): ValueInstance = {
    if(args.length != argValues.length) {
      throw new Exception(s"Function takes ${args.length} arguments but only ${argValues.length} were provided.")
    }

    state.variables.enterFrame()
    for((variableName, variableValue) <- args.zip(argValues)) {
      state.variables.setFrameVariable(variableName.name, variableValue)
    }

    for(statement <- body) {
      statement.execute(state)
    }
    state.variables.exitScope()

    IntValue(0) // Dummy return value for now
  }

  override def toString: String = "<function>"
}