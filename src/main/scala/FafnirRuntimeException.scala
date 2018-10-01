import scala.util.parsing.input.Positional

/**
  * Represents a fatal runtime error that couldn't have been caught while building the AST. These should always be
  * thrown up to the user. Reports the location of the error as accurately as possible. Note: must use `toString`
  * to get the location reported.
  * @param token The token determined to be the cause of the error (or closest possible).
  * @param msg Description of the issue.
  */
class FafnirRuntimeException(token: Positional, msg: String) extends Exception(msg) {
  override def toString: String = {
    s"Runtime error at ${token.pos.line}.${token.pos.column}: $msg"
  }
}

/**
  * Represents an illegal operation. Used by `ValueInstance`s, as they have no information about locations. The
  * evaluator can then transform this into a `FafnirRuntimeException` so that the location can be properly reported.
  */
class FafnirOperationException(msg: String) extends Exception(msg) {}

/**
  * Represents attempts to use an incompatible type for something.
  */
class FafnirIncompatibleTypeException(msg: String) extends Exception(msg) {}