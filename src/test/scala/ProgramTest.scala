class ProgramTest extends TestBase {
  test("Programs produce the correct final state") {
    val parser = new FafnirParser()
    val program =
      """
        |// Comment
        |var x: Int = 4 + (3 - 1) * 2; // End of line comments work too
        |if(x)
        |{
        |  var y: Int = 2;
        |  x = x + y;
        |}
        |
        |if(x - 10) {
        |  var notReached: String = "foo";
        |}
        |else { x = x + 1; }
        |
        |var z: String = "Something new";
        |{
        |  var local: String = "Will be out of scope at end of program";
        |  z = z + " or old";
        |}
        |
        |var loopCount: Int = 0;
        |while(loopCount - 5) {
        |  loopCount = loopCount + 1;
        |}
        |
        |// Loop we never go into
        |while(0) { loopCount = -1; }
        |
        |// Testing function definitions
        |var returnValue: Int = 0;
        |func foo(a, b) {
        |  returnValue = a + b;
        |}
        |foo(5, 7);
        |var renamedFoo: Function = foo;
        |if(renamedFoo(returnValue, 1)) {} // Evaluating function in an expression
        |
        |// Returning functions
        |func square(a) { return a * a; }
        |var thirty: Int = square(5) + 5;
      """.stripMargin

    val expectedVariables = Map(
      "x" -> IntValue(11),
      "z" -> StringValue("Something new or old"),
      "loopCount" -> IntValue(5),
      "returnValue" -> IntValue(13),
      "thirty" -> IntValue(30),
    )

    val expectedFunctions = Map(
      "foo" -> List("a", "b"),
      "renamedFoo" -> List("a", "b"),
      "square" -> List("a"),
    )

    doParse[Program](parser, parser.program, program) { matched =>
      val state = matched.execute()

      // We want to compare functions and non-function variables separately because functions aren't really
      // comparable (so we'll just compare the name and argument list).
      val variablesMap = state.variables.allVariables
      val (functionVariables, nonFunctionVariables) = variablesMap.partition(_._2.isInstanceOf[FunctionValue])
      val functionVariablesToArgs = functionVariables.map({
        case (name, value) => (name, value.asInstanceOf[FunctionValue].parameters.map(_.name))
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
      val programWithDecalaredVariable = "var a: Int = 0;\n" + p
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
        |var x: Int = 4 + (3 - 1) * 2;
        |{var y:Int = 9;}
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
        |func someFunction (a, b) { var local : Int = a + b; }
        |someFunction(1+2, 3);
      """.stripMargin

    val expectedOutput =
      """
        |var x: Int = 4 + ((3 - 1) * 2);
        |{
        |  var y: Int = 9;
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
        |  var local: Int = a + b;
        |}
        |someFunction(1 + 2, 3);
      """.stripMargin.trim

    doParse[Program](parser, parser.program, program) { matched =>
      assert(matched.toString === expectedOutput)
    }
  }

  test("Recursion works") {
    val parser = new FafnirParser()
    val program =
      """
        |// This is slightly complicated due to the lack of comparison operators
        |func fibonacci(n) {
        |  // Assumes n >= 0
        |  if(n) { // n != 0
        |    if(n - 1) { // n != 1
        |      return fibonacci(n - 1) + fibonacci(n - 2);
        |    }
        |    else { // n == 1
        |      return 1;
        |    }
        |  }
        |  else { // n == 0
        |    return 0;
        |  }
        |}
        |
        |var f0: Int = fibonacci(0);
        |var f1: Int = fibonacci(1);
        |var f2: Int = fibonacci(2);
        |var f3: Int = fibonacci(3);
        |var f4: Int = fibonacci(4);
        |var f5: Int = fibonacci(5);
        |var f6: Int = fibonacci(6);
      """.stripMargin

    val expectedVariables = Map(
      "f0" -> IntValue(0),
      "f1" -> IntValue(1),
      "f2" -> IntValue(1),
      "f3" -> IntValue(2),
      "f4" -> IntValue(3),
      "f5" -> IntValue(5),
      "f6" -> IntValue(8),
    )

    doParse[Program](parser, parser.program, program) { matched =>
      val state = matched.execute()
      val nonFunctionVariables = state.variables.allVariables.filter(!_._2.isInstanceOf[FunctionValue])
      assert(nonFunctionVariables === expectedVariables)
    }
  }

  test("Infinite recursion is caught") {
    val parser = new FafnirParser()
    val program =
      """
        |func infinite() {
        |  infinite();
        |}
        |infinite();
      """.stripMargin

    doParse[Program](parser, parser.program, program) { matched =>
      val intercepted = intercept[FafnirRuntimeException] {
        matched.execute()
      }
      assert(intercepted.toString == "Runtime error at 3.3: Max recursion depth exceeded in function infinite")
    }
  }

  test("Void types are used when functions don't return a value") {
    val parser = new FafnirParser()
    val program =
      """
        |func mixup() { return 1 + 1; } // So we can see if return values are mishandled
        |func noReturn() {
        |  var a: Int = 123;
        |}
        |func returnNoExpression() {
        |  var a: Int = 123;
        |  return;
        |}
        |var expectedValue: Int = mixup();
        |var noReturnValue: Void = noReturn();
        |expectedValue = expectedValue + mixup();
        |var returnNoExpressionValue: Void = returnNoExpression();
        |expectedValue = expectedValue + mixup();
      """.stripMargin

    val expectedVariables = Map(
      "expectedValue" -> IntValue(6),
      "noReturnValue" -> VoidValue(),
      "returnNoExpressionValue" -> VoidValue(),
    )

    doParse[Program](parser, parser.program, program) { matched =>
      val state = matched.execute()
      val nonFunctionVariables = state.variables.allVariables.filter(!_._2.isInstanceOf[FunctionValue])
      assert(nonFunctionVariables === expectedVariables)
    }
  }
}
