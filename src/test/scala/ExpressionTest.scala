import org.scalatest.FunSuite

class ExpressionTest extends TestBase {
  test("Expressions evaluate to expected values") {
    val parser = new FafnirParser()
    val state = new ProgramState()
    state.variables("foo") = IntValue(7)
    state.variables("myVar") = StringValue("abc")

    // Input -> expected evaluation
    val inputs_to_outputs = Seq(
      ("4 + (3 - 1) * 2", IntValue(8)),
      ("\"hello\" * 2", StringValue("hellohello")),
      ("\"hello\" + \" \" + \"world\"", StringValue("hello world")),
      ("5 / 2 - 2", IntValue(0)),
      ("-5 * -5", IntValue(25)),
      ("5 - -2", IntValue(7)),
      ("5-2", IntValue(3)),
      ("5 - foo", IntValue(-2)),
      ("myVar * (foo - 4)", StringValue("abcabcabc")),
    )

    for(input_output <- inputs_to_outputs) {
      doParse[Expression](parser, parser.expression, input_output._1) { matched =>
        assert(matched.evaluate(state) === input_output._2)
      }
    }
  }

  test("Expressions pretty print") {
    val parser = new FafnirParser()

    // Input -> expected evaluation
    val inputs_to_outputs = Seq(
      ("4+3/(2-1)", "4 + (3 / (2 - 1))"),
      ("\"hello\"   * 2   ", "(\"hello\" * 2)"),
    )

    for(input_output <- inputs_to_outputs) {
      doParse[Expression](parser, parser.expression, input_output._1) { matched =>
        assert(matched.toString === input_output._2)
      }
    }
  }
}