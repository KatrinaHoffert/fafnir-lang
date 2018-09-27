import org.scalatest.FunSuite

class StatementTest extends FunSuite {
  test("Statements modify program state correctly") {
    val parser = new FafnirParser()
    val state = new ProgramState()

    // Input -> expected evaluation
    val statements = Seq(
      "var x = 4 + (3 - 1) * 2;",
      "var y = 9;",
      "var _Abc123 = \"Hello\" + \" world!\";",
      "y = 10;"
    )

    for(statement <- statements) {
      parser.parse(parser.statement, statement) match {
        case parser.Success(matched, _) => matched.execute(state)
        case parser.Failure(msg, _) => fail(s"Parse failure for input $statement: $msg")
        case parser.Error(msg, _) => fail(s"Parse error for input $statement: $msg")
      }
    }

    val expectedVariables = Map(
      "x" -> IntValue(8),
      "y" -> IntValue(10),
      "_Abc123" -> StringValue("Hello world!")
    )
    assert(state.globals === expectedVariables)
  }

  test("Assignment fails if variable not declared first") {
    val parser = new FafnirParser()
    val state = new ProgramState()
    parser.parse(parser.statement, "x = 123;") match {
      case parser.Success(matched, _) =>
        val intercepted = intercept[Exception] {
          matched.execute(state)
        }
        assert(intercepted.getMessage === "Assignment to undeclared variable x")
      case parser.Failure(msg, _) => fail(s"Parse failure: $msg")
      case parser.Error(msg, _) => fail(s"Parse error: $msg")
    }
  }

  test("Statements pretty print") {
    val parser = new FafnirParser()

    // Input -> expected evaluation
    val inputs_to_outputs = Seq(
      ("var x=123+4;", "var x = 123 + 4;"),
      ("x=123+4;", "x = 123 + 4;"),
    )

    for(input_output <- inputs_to_outputs) {
      parser.parse(parser.statement, input_output._1) match {
        case parser.Success(matched, _) => assert(matched.toString === input_output._2)
        case parser.Failure(msg, _) => fail(s"Parse failure for input ${input_output._1}: $msg")
        case parser.Error(msg, _) => fail(s"Parse error for input ${input_output._1}: $msg")
      }
    }
  }
}