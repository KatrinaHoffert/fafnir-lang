import org.scalatest.FunSuite

abstract class TestBase extends FunSuite {
  /**
    * Handles boilerplate in actually doing the parse with error handling.
    * @param parser The parser to use to parse the input with.
    * @param componentFunction The component parser function to try to parse the input to. This is something like
    *                          `parser.statement`.
    * @param input The input program we're parsing.
    * @param successCase If we succeed in parsing, this function will be given the matched component as returned by
    *                    the component function (eg, `Statement`). We can then do things like evaluate it and perform
    *                    assertions.
    * @tparam T The type of component (eg, `Statement`). This is the type given to the `successCase` function.
    */
  def doParse[T](parser: FafnirParser, componentFunction: Any, input: String)(successCase: T => Unit): Unit = {
    val componentTyped = componentFunction.asInstanceOf[parser.Parser[T]]
    parser.parse(componentTyped, input) match {
      case parser.Success(matched, _) =>
        successCase(matched)
      case parser.Failure(msg, _) => fail(s"Parse failure: $msg\n\nProgram was:\n$input")
      case parser.Error(msg, _) => fail(s"Parse error: $msg\n\nProgram was:\n$input")
    }
  }
}
