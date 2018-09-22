abstract class ValueInstance {
  def +(that: ValueInstance): ValueInstance
  def -(that: ValueInstance): ValueInstance
  def *(that: ValueInstance): ValueInstance
  def /(that: ValueInstance): ValueInstance
}

case class IntValue(value: Int) extends ValueInstance {
  def +(that: ValueInstance): ValueInstance = {
    that match {
      case intThat: IntValue => IntValue(this.value + intThat.value)
      case _ => throw new Exception("Not supported type")
    }
  }

  def -(that: ValueInstance): ValueInstance = {
    that match {
      case intThat: IntValue => IntValue(this.value - intThat.value)
      case _ => throw new Exception("Not supported type")
    }
  }

  def *(that: ValueInstance): ValueInstance = {
    that match {
      case intThat: IntValue => IntValue(this.value * intThat.value)
      case _ => throw new Exception("Not supported type")
    }
  }

  def /(that: ValueInstance): ValueInstance = {
    that match {
      case intThat: IntValue => IntValue(this.value / intThat.value)
      case _ => throw new Exception("Not supported type")
    }
  }

  override def toString: String = this.value.toString
}

case class StringValue(value: String) extends ValueInstance {
  def +(that: ValueInstance): ValueInstance = {
    that match {
      case stringThat: StringValue => StringValue(this.value + stringThat.value)
      case _ => throw new Exception("Not supported type")
    }
  }

  def -(that: ValueInstance): ValueInstance = {
    throw new Exception("Not supported type")
  }

  def *(that: ValueInstance): ValueInstance = {
    that match {
      case intThat: IntValue => StringValue(this.value * intThat.value)
      case _ => throw new Exception("Not supported type")
    }
  }

  def /(that: ValueInstance): ValueInstance = {
    throw new Exception("Not supported type")
  }

  override def toString: String = s""""${this.value}""""
}