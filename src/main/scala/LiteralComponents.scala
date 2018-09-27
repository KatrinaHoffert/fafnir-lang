case class Identifier(name: String) extends Primary {
  /**
    * Identifiers can be evaluated as primaries when they exist in an expression. In which case, the evaluated value
    * is the value of said variable. If not in the program state, it's an undefined variable and triggers an error.
    */
  override def evaluate(state: ProgramState): ValueInstance = {
    state.globals.get(name) match {
      case Some(value) => value
      case None => throw new Exception(s"Variable $name not defined")
    }
  }
  override def toString: String = name
}

case class StringLiteral(x: String) extends Primary {
  override def evaluate(state: ProgramState): ValueInstance = StringValue(x)
  override def toString: String = s""""$x""""
}

case class IntLiteral(x: Int) extends Primary {
  override def evaluate(state: ProgramState): ValueInstance = IntValue(x)
  override def toString: String = s"$x"
}

abstract class Evaluable {
  def evaluate(state: ProgramState): ValueInstance
}

abstract class Expression extends Evaluable
abstract class Term extends Expression
abstract class Primary extends Term