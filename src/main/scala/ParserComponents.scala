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

case class AdditionEvaluable(x: Evaluable, y: Evaluable) extends Expression {
  override def evaluate: ValueInstance = x.evaluate + y.evaluate
  override def toString: String = s"$x + $y"
}

case class SubtractionEvaluable(x: Evaluable, y: Evaluable) extends Expression {
  override def evaluate: ValueInstance = x.evaluate - y.evaluate
  override def toString: String = s"$x - $y"
}

case class Braces(expression: Expression) extends Primary {
  override def evaluate: ValueInstance = expression.evaluate
  override def toString: String = s"($expression)"
}

case class MultiplicationTerm(x: Evaluable, y: Evaluable) extends Term {
  override def evaluate: ValueInstance = x.evaluate * y.evaluate
  override def toString: String = s"($x * $y)"
}

case class DivisionTerm(x: Evaluable, y: Evaluable) extends Term {
  override def evaluate: ValueInstance = x.evaluate / y.evaluate
  override def toString: String = s"($x / $y)"
}

abstract class Evaluable {
  def evaluate: ValueInstance
}
abstract class Expression extends Evaluable
abstract class Term extends Expression
abstract class Primary extends Term