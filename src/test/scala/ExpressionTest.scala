class ExpressionTest extends TestBase {
  test("Expressions evaluate to expected values") {
    val parser = new FafnirParser()
    val state = new ProgramState()
    state.variables("foo") = IntValue(7)
    state.variables("myVar") = StringValue("abc")

    // Input -> expected evaluation
    val inputsToOutputs = Seq(
      ("4 + (3 - 1) * 2", IntValue(8)),
      ("\"hello\" * 2", StringValue("hellohello")),
      ("\"hello\" + \" \" + \"world\"", StringValue("hello world")),
      ("5 / 2 - 2", IntValue(0)),
      ("-5 * -5", IntValue(25)),
      ("5 - -2", IntValue(7)),
      ("5-2", IntValue(3)),
      ("5 - foo", IntValue(-2)),
      ("myVar * (foo - 4)", StringValue("abcabcabc")),
      (""""Escape\" \\\"test"""", StringValue("""Escape" \"test""")),
      (""""More\n\tescapes"""", StringValue("More\n\tescapes")),
    )

    for(inputOutput <- inputsToOutputs) {
      doParse[Expression](parser, parser.expression, inputOutput._1) { matched =>
        assert(matched.evaluate(state) === inputOutput._2)
      }
    }
  }

  test("Expressions pretty print") {
    val parser = new FafnirParser()

    // Input -> expected evaluation
    val inputsToOutputs = Seq(
      ("4+3/(2-1)", "4 + (3 / (2 - 1))"),
      ("\"hello\"   * 2   ", "(\"hello\" * 2)"),
    )

    for(inputOutput <- inputsToOutputs) {
      doParse[Expression](parser, parser.expression, inputOutput._1) { matched =>
        assert(matched.toString === inputOutput._2)
      }
    }
  }

  test("Unsupported operations get the expected error message") {
    val parser = new FafnirParser()
    val state = new ProgramState()
    state.variables("predefinedInt") = IntValue(123)
    state.variables("someFunction") = FunctionValue(Identifier("someFunction"), List(Identifier("a")), List())

    // Input -> error message
    val inputsToErrorMessages = Seq(
      //("\"hello\" + 2", "Runtime error at 1.11: Operation + is not defined on types String and Int"),
      //("\n\n(\"hi\" + \"hi\") / 2", "Runtime error at 3.17: Operation / is not defined on types String and Int"),
      ("5 / 0\n\n", "Runtime error at 1.5: Division by zero"),
      ("predefinedInt()", "Runtime error at 1.1: Type Int is not callable"),
      ("someFunction()", "Runtime error at 1.1: Function someFunction takes 1 arguments but 0 were provided"),
      ("someFunction(1, 2)", "Runtime error at 1.1: Function someFunction takes 1 arguments but 2 were provided"),
    )

    for(inputToErrorMessage <- inputsToErrorMessages) {
      doParse[Expression](parser, parser.expression, inputToErrorMessage._1) { matched =>
        val intercepted = intercept[FafnirRuntimeException] {
          matched.evaluate(state)
        }
        assert(intercepted.toString === inputToErrorMessage._2)
      }
    }
  }
}