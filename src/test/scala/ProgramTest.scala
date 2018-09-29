import org.scalatest.FunSuite

class ProgramTest extends TestBase {
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
        |
        |// Testing function definitions
        |var returnValue = 0;
        |func foo(a, b) {
        |  returnValue = a + b;
        |}
        |foo(5, 7);
        |var renamedFoo = foo;
        |if(renamedFoo(returnValue, 1)) {} // Evaluating function in an expression
      """.stripMargin

    val expectedVariables = Map(
      "x" -> IntValue(11),
      "z" -> StringValue("Something new or old"),
      "loopCount" -> IntValue(5),
      "returnValue" -> IntValue(13)
    )

    val expectedFunctions = Map(
      "foo" -> List("a", "b"),
      "renamedFoo" -> List("a", "b"),
    )

    doParse[Program](parser, parser.program, program) { matched =>
      val state = matched.execute()

      // We want to compare functions and non-function variables separately because functions aren't really
      // comparable (so we'll just compare the name and argument list).
      val variablesMap = state.variables.allVariables
      val (functionVariables, nonFunctionVariables) = variablesMap.partition(_._2.isInstanceOf[FunctionValue])
      val functionVariablesToArgs = functionVariables.map({
        case (name, value) => (name, value.asInstanceOf[FunctionValue].args.map(_.name))
      })

      assert(nonFunctionVariables === expectedVariables)
      assert(functionVariablesToArgs === expectedFunctions)
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
      doParse[Program](parser, parser.program, programWithDecalaredVariable) { matched =>
        val state = matched.execute()
        assert(state.variables.allVariables("a") === IntValue(1), s"Program: $programWithDecalaredVariable")
      }
    }
  }

  test("Programs pretty print") {
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
        |func someFunction (a, b) { var local = a + b; }
        |someFunction(1+2, 3);
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
        |func someFunction(a, b) {
        |  var local = a + b;
        |}
        |someFunction(1 + 2, 3);
      """.stripMargin.trim

    doParse[Program](parser, parser.program, program) { matched =>
      assert(matched.toString === expectedOutput)
    }
  }
}