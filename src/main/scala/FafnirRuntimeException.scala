import scala.util.parsing.input.Positional

class FafnirRuntimeException(token: Positional, msg: String) extends Exception(msg) {
  override def toString: String = {
    s"Runtime error at ${token.pos.line}.${token.pos.column}: $msg"
  }
}

class FafnirOperationException(msg: String) extends Exception(msg) {}