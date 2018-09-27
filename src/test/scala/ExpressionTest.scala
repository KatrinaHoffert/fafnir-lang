import org.scalatest.FunSuite

class ExpressionTest extends FunSuite {
  test("Expressions evaluate to expected values") {
    val parser = new FafnirParser()

    // Input -> expected evaluation
    val inputs_to_outputs = Seq(
      ("4 + (3 - 1) * 2", IntValue(8)),
      ("\"hello\" * 2", StringValue("hellohello")),
      ("\"hello\" + \" \" + \"world\"", StringValue("hello world")),
      ("5 / 2 - 2", IntValue(0)),
    )

    for(input_output <- inputs_to_outputs) {
      parser.parse(parser.expression, input_output._1) match {
        case parser.Success(matched, _) => assert(matched.evaluate === input_output._2)
        case parser.Failure(msg, _) => fail(s"Parse failure: $msg")
        case parser.Error(msg, _) => fail(s"Parse error: $msg")
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
      parser.parse(parser.expression, input_output._1) match {
        case parser.Success(matched, _) => assert(matched.toString === input_output._2)
        case parser.Failure(msg, _) => fail(s"Parse failure: $msg")
        case parser.Error(msg, _) => fail(s"Parse error: $msg")
      }
    }
  }
}