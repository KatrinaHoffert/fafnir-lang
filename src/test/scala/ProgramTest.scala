import org.scalatest.FunSuite

class ProgramTest extends FunSuite {
  test("Programs produce the correct final state") {
    val parser = new FafnirParser()

    val program = """
      var x = 4 + (3 - 1) * 2;
      {
        var y = 2;
        x = x + y;
      }
      var z = "Something new";
    """

    val expectedVariables = Map(
      "x" -> IntValue(10),
      "z" -> StringValue("Something new"),
    )

    parser.parse(parser.program, program) match {
      case parser.Success(matched, _) =>
        val state = matched.execute()
        assert(state.variables.allVariables === expectedVariables)
      case parser.Failure(msg, _) => fail(s"Parse failure: $msg")
      case parser.Error(msg, _) => fail(s"Parse error: $msg")
    }
  }

  test("Statements pretty print") {
    val parser = new FafnirParser()

    val program = """
      var x = 4 + (3 - 1) * 2;
      {var y = 9;}
      y = 10;
    """

    val expectedOutput = """
var x = 4 + ((3 - 1) * 2);
{
  var y = 9;
}
y = 10;
    """.trim

    parser.parse(parser.program, program) match {
      case parser.Success(matched, _) => assert(matched.toString === expectedOutput)
      case parser.Failure(msg, _) => fail(s"Parse failure: $msg")
      case parser.Error(msg, _) => fail(s"Parse error: $msg")
    }
  }
}