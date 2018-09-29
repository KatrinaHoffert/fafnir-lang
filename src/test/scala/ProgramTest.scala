import org.scalatest.FunSuite

class ProgramTest extends FunSuite {
  test("Programs produce the correct final state") {
    val parser = new FafnirParser()

    val program =
      """
        |// Comment
        |var x = 4 + (3 - 1) * 2; // End of line comments work too
        |if(x)
        |{
        |  var y = 2;
        |  x = x + y;
        |}
        |
        |if(x - 10) {
        |  var notReached = "foo";
        |}
        |else { x = x + 1; }
        |
        |var z = "Something new";
        |{
        |  var local = "Will be out of scope at end of program";
        |  z = z + " or old";
        |}
        |
        |var loopCount = 0;
        |while(loopCount - 5) {
        |  loopCount = loopCount + 1;
        |}
        |
        |// Loop we never go into
        |while(0) { loopCount = -1; }
      """.stripMargin

    val expectedVariables = Map(
      "x" -> IntValue(11),
      "z" -> StringValue("Something new or old"),
      "loopCount" -> IntValue(5),
    )

    parser.parse(parser.program, program) match {
      case parser.Success(matched, _) =>
        val state = matched.execute()
        assert(state.variables.allVariables === expectedVariables)
      case parser.Failure(msg, _) => fail(s"Parse failure: $msg")
      case parser.Error(msg, _) => fail(s"Parse error: $msg")
    }
  }

  test("If statements are taken as expected") {
    val parser = new FafnirParser()

    // All these programs should result in `a` being assigned "1" (it defaults to 0). Taking any wrong path would result
    // in it having a different value.
    val programs = Seq(
      """
        |if(1) { a = 1; }
        |elif(1 + 2) { a = 123; }
        |elif(0 / 0) { a = 123; }
        |else { a = 123; }
      """.stripMargin,
      """
        |if (a) {
        |  a = 123;
        |} elif (0) {
        |  a = 123;
        |} elif (a + 1) {
        |  a = 1;
        |}
      """.stripMargin,
      """
        |if(0){a=123;}
        |elif(0){a=123;}
        |else{a=1;}
      """.stripMargin,
      """
        |if (0) {
        |  a = 123;
        |} elif (0) {
        |  a = 123;
        |} elif (0) {
        |  a = 123;
        |}
        |else {
        |  a = 1;
        |}
      """.stripMargin,
      """
        |if (1) {
        |  a = 1;
        |}
      """.stripMargin,
      """
        |if (0) {
        |  a = 123;
        |}
        |a = a + 1;
      """.stripMargin,
      """
        |if (1) {
        |  if(0)
        |  {
        |    a = 123;
        |  }
        |  else
        |  {
        |    if(1){ a = 1; }
        |  }
        |}
      """.stripMargin
    )

    for(p <- programs) {
      val programWithDecalaredVariable = "var a = 0;\n" + p
      parser.parse(parser.program, programWithDecalaredVariable) match {
        case parser.Success(matched, _) =>
          val state = matched.execute()
          assert(state.variables.allVariables("a") === IntValue(1), s"Program: $programWithDecalaredVariable")
        case parser.Failure(msg, _) => fail(s"Parse failure: $msg; Program: $programWithDecalaredVariable")
        case parser.Error(msg, _) => fail(s"Parse error: $msg; Program: $programWithDecalaredVariable")
      }
    }
  }

  test("Statements pretty print") {
    val parser = new FafnirParser()

    val program =
      """
        |var x = 4 + (3 - 1) * 2;
        |{var y = 9;}
        |y = 10;
        |if (1) {
        |  if(0)
        |  {
        |    y = 123;
        |  }
        |  elif ( x + y) {
        |    y = 123;
        |  }
        |  elif (x - y) {
        |    y = 321; }
        |  else
        |  {
        |    if(1){ a = 1; }
        |  }
        |}
      """.stripMargin

    val expectedOutput =
      """
        |var x = 4 + ((3 - 1) * 2);
        |{
        |  var y = 9;
        |}
        |y = 10;
        |if(1) {
        |  if(0) {
        |    y = 123;
        |  }
        |  elif(x + y) {
        |    y = 123;
        |  }
        |  elif(x - y) {
        |    y = 321;
        |  }
        |  else {
        |    if(1) {
        |      a = 1;
        |    }
        |  }
        |}
      """.stripMargin.trim

    parser.parse(parser.program, program) match {
      case parser.Success(matched, _) => assert(matched.toString === expectedOutput)
      case parser.Failure(msg, _) => fail(s"Parse failure: $msg")
      case parser.Error(msg, _) => fail(s"Parse error: $msg")
    }
  }
}