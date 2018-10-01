class ScopesTest extends TestBase {
  test("Variables are resolvable within their scopes (but not others)") {
    val scopes = new Scopes()
    scopes("foo") = VariableInfo("Int", IntValue(1))
    assert(scopes("foo") === VariableInfo("Int", IntValue(1)))

    scopes.enterScope()
    scopes("bar") = VariableInfo("Int", IntValue(1))
    assert(scopes("foo") === VariableInfo("Int", IntValue(1)))
    assert(scopes("bar") === VariableInfo("Int", IntValue(1)))

    // Double nested scope
    scopes.enterScope()
    scopes("baz") = VariableInfo("Int", IntValue(1))
    assert(scopes("foo") === VariableInfo("Int", IntValue(1)))
    assert(scopes("bar") === VariableInfo("Int", IntValue(1)))
    assert(scopes("baz") === VariableInfo("Int", IntValue(1)))
    scopes.exitScope()

    // Baz is now out of scope
    assert(scopes("foo") === VariableInfo("Int", IntValue(1)))
    assert(scopes("bar") === VariableInfo("Int", IntValue(1)))
    assertVariableNotDefined(scopes, "baz")
    scopes.exitScope()

    // Bar is now out of scope, too
    assert(scopes("foo") === VariableInfo("Int", IntValue(1)))
    assertVariableNotDefined(scopes, "bar")
  }

  test("Multiple frames can access their local scopes, but not each other") {
    val scopes = new Scopes()
    scopes.enterFrame()
    scopes("topLevel") = VariableInfo("Int", IntValue(1))
    assert(scopes("topLevel") === VariableInfo("Int", IntValue(1)))

    scopes.enterScope()
    scopes("topLevelScope") = VariableInfo("Int", IntValue(1))
    assert(scopes("topLevel") === VariableInfo("Int", IntValue(1)))
    assert(scopes("topLevelScope") === VariableInfo("Int", IntValue(1)))

    scopes.enterFrame()
    scopes("anotherLevel") = VariableInfo("Int", IntValue(1))
    assert(scopes("anotherLevel") === VariableInfo("Int", IntValue(1)))

    scopes.enterScope()
    scopes("anotherLevelScope") = VariableInfo("Int", IntValue(1))
    assert(scopes("anotherLevel") === VariableInfo("Int", IntValue(1)))
    assert(scopes("anotherLevelScope") === VariableInfo("Int", IntValue(1)))
    assertVariableNotDefined(scopes, "topLevel")
    assertVariableNotDefined(scopes, "topLevelScope")
    scopes.exitScope()

    assertVariableNotDefined(scopes, "anotherLevelScope")
    scopes.exitScope()

    // Should now be back in the first frame
    assertVariableNotDefined(scopes, "anotherLevel")
    assert(scopes("topLevel") === VariableInfo("Int", IntValue(1)))
    assert(scopes("topLevelScope") === VariableInfo("Int", IntValue(1)))

    scopes.exitScope()
    assertVariableNotDefined(scopes, "topLevelScope")

    scopes.exitScope()

    // Now not in a frame
    assertVariableNotDefined(scopes, "topLevel")
  }

  test("Ghosting happens as expected") {
    val scopes = new Scopes()
    scopes("a") = VariableInfo("Int", IntValue(1))
    scopes("b") = VariableInfo("Int", IntValue(-1))
    assert(scopes("a") === VariableInfo("Int", IntValue(1)))
    assert(scopes("b") === VariableInfo("Int", IntValue(-1)))

    scopes.enterFrame()
    scopes.setFrameVariable("a", VariableInfo("Int", IntValue(1))) // Ghosts the original "a"
    assert(scopes("a") === VariableInfo("Int", IntValue(1)))

    scopes.enterFrame()
    scopes.setFrameVariable("a", VariableInfo("Int", IntValue(1))) // Ghosts the global "a" *and* is distinct from other frame's
    assert(scopes("a") === VariableInfo("Int", IntValue(1)))
    scopes("b") = VariableInfo("Int", IntValue(-2)) // Editing a global inside a frame
    scopes("a") = VariableInfo("Int", IntValue(1)) // Changes only the local frame variable
    assert(scopes("a") === VariableInfo("Int", IntValue(1)))
    scopes.exitScope()

    // Back in the first frame
    assert(scopes("a") === VariableInfo("Int", IntValue(1)))
    scopes.exitScope()

    // Not in a frame anymore
    assert(scopes("a") === VariableInfo("Int", IntValue(1)))
    assert(scopes("b") === VariableInfo("Int", IntValue(-2)))
  }

  def assertVariableNotDefined(scopes: Scopes, variableName: String): Unit = {
    val intercepted = intercept[NoSuchElementException] {
      scopes(variableName)
    }
    assert(intercepted.getMessage === s"Variable $variableName is not defined")
  }
}