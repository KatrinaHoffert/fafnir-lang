case class AdditionEvaluable(x: Evaluable, y: Evaluable) extends Expression {
  override def evaluate(state: ProgramState): ValueInstance = {
    try {
      x.evaluate(state) + y.evaluate(state)
    }
    catch {
      case ex: FafnirOperationException => throw new FafnirRuntimeException(y, ex.getMessage)
    }
  }
  override def toString: String = s"$x + $y"
}

case class SubtractionEvaluable(x: Evaluable, y: Evaluable) extends Expression {
  override def evaluate(state: ProgramState): ValueInstance = {
    try {
      x.evaluate(state) - y.evaluate(state)
    }
    catch {
      case ex: FafnirOperationException => throw new FafnirRuntimeException(y, ex.getMessage)
    }
  }
  override def toString: String = s"$x - $y"
}

case class Braces(expression: Expression) extends Primary {
  override def evaluate(state: ProgramState): ValueInstance = {
    try {
      expression.evaluate(state)
    }
    catch {
      case ex: FafnirOperationException => throw new FafnirRuntimeException(expression, ex.getMessage)
    }
  }
  override def toString: String = s"($expression)"
}

case class MultiplicationTerm(x: Evaluable, y: Evaluable) extends Term {
  override def evaluate(state: ProgramState): ValueInstance = {
    try {
      x.evaluate(state) * y.evaluate(state)
    }
    catch {
      case ex: FafnirOperationException => throw new FafnirRuntimeException(y, ex.getMessage)
    }
  }
  override def toString: String = s"($x * $y)"
}

case class DivisionTerm(x: Evaluable, y: Evaluable) extends Term {
  override def evaluate(state: ProgramState): ValueInstance = {
    try {
      x.evaluate(state) / y.evaluate(state)
    }
    catch {
      case ex: FafnirOperationException => throw new FafnirRuntimeException(y, ex.getMessage)
    }
  }
  override def toString: String = s"($x / $y)"
}

case class FunctionCall(identifier: Identifier, argExpressions: List[Expression]) extends Primary {
  override def evaluate(state: ProgramState): ValueInstance = {
    try {
      // Note: FafnirOperationException should never be thrown by the argument evaluation since a more expression
      // should catch it and transform it into a FafnirRuntimeException
      val argValues = argExpressions.map(_.evaluate(state))
      state.variables(identifier.name).value.call(state, argValues)
    }
    catch {
      case ex: FafnirOperationException => throw new FafnirRuntimeException(identifier, ex.getMessage)
    }
  }
  override def toString: String = s"$identifier(${argExpressions.map(_.toString).mkString(", ")})"
}