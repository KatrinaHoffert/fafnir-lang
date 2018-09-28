abstract class ValueInstance {
  def +(that: ValueInstance): ValueInstance
  def -(that: ValueInstance): ValueInstance
  def *(that: ValueInstance): ValueInstance
  def /(that: ValueInstance): ValueInstance
  def isTruthy: Boolean
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

  override def -(that: ValueInstance): ValueInstance = {
    throw new Exception("Not supported type")
  }

  override def *(that: ValueInstance): ValueInstance = {
    that match {
      case intThat: IntValue => StringValue(value * intThat.value)
      case _ => throw new Exception("Not supported type")
    }
  }

  override def /(that: ValueInstance): ValueInstance = {
    throw new Exception("Not supported type")
  }

  override def isTruthy: Boolean = value.length > 0

  override def toString: String = s""""$value""""
}