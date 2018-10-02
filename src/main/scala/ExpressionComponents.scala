case class AdditionEvaluable(x: Evaluable, y: Evaluable) extends Expression {
  override def evaluate(state: ProgramState): ValueInstance = {
    try {
      val xValue = x.evaluate(state)
      val yValue = y.evaluate(state)
      xValue.methodMap("__add", List(yValue.typeName)).call(state, List(yValue))
    }
    catch {
      case ex: FafnirOperationException => throw new FafnirRuntimeException(y, ex.getMessage)
    }
  }

  def evaluateType(staticInfo: StaticInfo): String = {
    // Hack: expressions evaluating to first type
    x.evaluateType(staticInfo)
  }

  override def toString: String = s"$x + $y"
}

case class SubtractionEvaluable(x: Evaluable, y: Evaluable) extends Expression {
  override def evaluate(state: ProgramState): ValueInstance = {
    try {
      val xValue = x.evaluate(state)
      val yValue = y.evaluate(state)
      xValue.methodMap("__sub", List(yValue.typeName)).call(state, List(yValue))
    }
    catch {
      case ex: FafnirOperationException => throw new FafnirRuntimeException(y, ex.getMessage)
    }
  }

  def evaluateType(staticInfo: StaticInfo): String = {
    // Hack: expressions evaluating to first type
    x.evaluateType(staticInfo)
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

  def evaluateType(staticInfo: StaticInfo): String = {
    // Hack: expressions evaluating to first type
    expression.evaluateType(staticInfo)
  }

  override def toString: String = s"($expression)"
}

case class MultiplicationTerm(x: Evaluable, y: Evaluable) extends Term {
  override def evaluate(state: ProgramState): ValueInstance = {
    try {
      val xValue = x.evaluate(state)
      val yValue = y.evaluate(state)
      xValue.methodMap("__mult", List(yValue.typeName)).call(state, List(yValue))
    }
    catch {
      case ex: FafnirOperationException => throw new FafnirRuntimeException(y, ex.getMessage)
    }
  }

  def evaluateType(staticInfo: StaticInfo): String = {
    // Hack: expressions evaluating to first type
    x.evaluateType(staticInfo)
  }

  override def toString: String = s"($x * $y)"
}

case class DivisionTerm(x: Evaluable, y: Evaluable) extends Term {
  override def evaluate(state: ProgramState): ValueInstance = {
    try {
      val xValue = x.evaluate(state)
      val yValue = y.evaluate(state)
      xValue.methodMap("__div", List(yValue.typeName)).call(state, List(yValue))
    }
    catch {
      case ex: FafnirOperationException => throw new FafnirRuntimeException(y, ex.getMessage)
    }
  }

  def evaluateType(staticInfo: StaticInfo): String = {
    // Hack: expressions evaluating to first type
    x.evaluateType(staticInfo)
  }

  override def toString: String = s"($x / $y)"
}

case class FunctionCall(identifier: Identifier, argExpressions: List[Expression]) extends Primary {
  override def evaluate(state: ProgramState): ValueInstance = {
    try {
      // Note: FafnirOperationException should never be thrown by the argument evaluation since a more expression
      // should catch it and transform it into a FafnirRuntimeException
      val argValues = argExpressions.map(_.evaluate(state))
      state.variables(identifier.name).call(state, argValues)
    }
    catch {
      case ex: FafnirOperationException => throw new FafnirRuntimeException(identifier, ex.getMessage)
    }
  }

  def evaluateType(staticInfo: StaticInfo): String = {
    // TODO: Functions need to specify their return type
    "Int"
  }

  override def toString: String = s"$identifier(${argExpressions.map(_.toString).mkString(", ")})"
}