case class Identifier(name: String) {
  override def toString: String = name
}

case class StringLiteral(x: String) extends Primary {
  override def evaluate: ValueInstance = StringValue(x)
  override def toString: String = s""""$x""""
}

case class IntLiteral(x: Int) extends Primary {
  override def evaluate: ValueInstance = IntValue(x)
  override def toString: String = s"$x"
}

abstract class Evaluable {
  def evaluate: ValueInstance
}

abstract class Expression extends Evaluable
abstract class Term extends Expression
abstract class Primary extends Term