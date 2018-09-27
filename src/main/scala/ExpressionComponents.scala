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