case class AdditionEvaluable(x: Evaluable, y: Evaluable) extends Expression {
  override def evaluate(state: ProgramState): ValueInstance = x.evaluate(state) + y.evaluate(state)
  override def toString: String = s"$x + $y"
}

case class SubtractionEvaluable(x: Evaluable, y: Evaluable) extends Expression {
  override def evaluate(state: ProgramState): ValueInstance = x.evaluate(state) - y.evaluate(state)
  override def toString: String = s"$x - $y"
}

case class Braces(expression: Expression) extends Primary {
  override def evaluate(state: ProgramState): ValueInstance = expression.evaluate(state)
  override def toString: String = s"($expression)"
}

case class MultiplicationTerm(x: Evaluable, y: Evaluable) extends Term {
  override def evaluate(state: ProgramState): ValueInstance = x.evaluate(state) * y.evaluate(state)
  override def toString: String = s"($x * $y)"
}

case class DivisionTerm(x: Evaluable, y: Evaluable) extends Term {
  override def evaluate(state: ProgramState): ValueInstance = x.evaluate(state) / y.evaluate(state)
  override def toString: String = s"($x / $y)"
}

case class FunctionCall(identifier: Identifier, argExpressions: List[Expression]) extends Primary {
  override def evaluate(state: ProgramState): ValueInstance = {
    val argValues = argExpressions.map(_.evaluate(state))
    state.variables(identifier.name).call(state, argValues)
  }
  override def toString: String = s"$identifier(${argExpressions.map(_.toString).mkString(", ")})"
}