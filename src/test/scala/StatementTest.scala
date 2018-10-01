import org.scalatest.FunSuite

class StatementTest extends TestBase {
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
      doParse[Statement](parser, parser.statement, statement) { matched =>
        matched.execute(state)
      }
    }

    val expectedVariables = Map(
      "x" -> IntValue(8),
      "y" -> IntValue(10),
      "_Abc123" -> StringValue("Hello world!")
    )
    assert(state.variables.allVariables === expectedVariables)
  }

  test("Assignment fails if variable not declared first") {
    val parser = new FafnirParser()
    val state = new ProgramState()
    doParse[Statement](parser, parser.statement, "x = 123;") { matched =>
      val intercepted = intercept[FafnirRuntimeException] {
        matched.execute(state)
      }
      assert(intercepted.getMessage === "Assignment to undeclared variable x")
    }
  }

  test("Functions cannot be declared multiple times") {
    val parser = new FafnirParser()
    val state = new ProgramState()

    doParse[Statement](parser, parser.statement, "func foo() {}") { matched =>
      matched.execute(state)
      assert(state.variables.contains("foo"), "First declaration should succeed")
    }
    doParse[Statement](parser, parser.statement, "func foo(bar) {}") { matched =>
      val intercepted = intercept[FafnirRuntimeException] {
        matched.execute(state)
      }
      assert(intercepted.getMessage === "Cannot assign new function to existing variable foo")
    }
  }

  test("Statements pretty print") {
    val parser = new FafnirParser()

    // Input -> expected evaluation
    val inputsToOutputs = Seq(
      ("var x=123+4;", "var x = 123 + 4;"),
      ("x=123+4;", "x = 123 + 4;"),
    )

    for(inputOutput <- inputsToOutputs) {
      doParse[Statement](parser, parser.statement, inputOutput._1) { matched =>
        assert(matched.toString === inputOutput._2)
      }
    }
  }
}