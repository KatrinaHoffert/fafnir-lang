import java.util.NoSuchElementException

import scala.util.parsing.input.Positional

case class Identifier(name: String) extends Primary {
  /**
    * Identifiers can be evaluated as primaries when they exist in an expression. In which case, the evaluated value
    * is the value of said variable. If not in the program state, it's an undefined variable and triggers an error.
    */
  override def evaluate(state: ProgramState): ValueInstance = {
    state.variables(name)
  }

  def evaluateType(staticInfo: StaticInfo): String = {
    val (_, variableType) = staticInfo.getFullyQualifiedVariableNameAndType(name)

    variableType match {
      case Some(actualVariableType) => actualVariableType
      case None => throw new FafnirRuntimeException(this, s"Variable $name is not defined")
    }
  }
  override def toString: String = name
}

case class StringLiteral(x: String) extends Primary {
  override def evaluate(state: ProgramState): ValueInstance = StringValue(x)
  def evaluateType(staticInfo: StaticInfo): String = "String"
  override def toString: String = s""""$x""""
}

case class IntLiteral(x: Int) extends Primary {
  override def evaluate(state: ProgramState): ValueInstance = IntValue(x)
  def evaluateType(staticInfo: StaticInfo): String = "Int"
  override def toString: String = s"$x"
}

abstract class Evaluable extends Positional {
  def evaluate(state: ProgramState): ValueInstance
  def evaluateType(staticInfo: StaticInfo): String
}

abstract class Expression extends Evaluable
abstract class Term extends Expression
abstract class Primary extends Term