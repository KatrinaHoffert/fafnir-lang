import org.scalatest.FunSuite

class ScopesTest extends FunSuite {
  test("Variables are resolvable within their scopes (but not others)") {
    val scopes = new Scopes()
    scopes("foo") = IntValue(1)
    assert(scopes("foo") === IntValue(1))

    scopes.enterScope()
    scopes("bar") = IntValue(2)
    assert(scopes("foo") === IntValue(1))
    assert(scopes("bar") === IntValue(2))

    // Double nested scope
    scopes.enterScope()
    scopes("baz") = IntValue(3)
    assert(scopes("foo") === IntValue(1))
    assert(scopes("bar") === IntValue(2))
    assert(scopes("baz") === IntValue(3))
    scopes.exitScope()

    // Baz is now out of scope
    assert(scopes("foo") === IntValue(1))
    assert(scopes("bar") === IntValue(2))
    assertVariableNotDefined(scopes, "baz")
    scopes.exitScope()

    // Bar is now out of scope, too
    assert(scopes("foo") === IntValue(1))
    assertVariableNotDefined(scopes, "bar")
  }

  test("Multiple frames can access their local scopes, but not each other") {
    val scopes = new Scopes()
    scopes.enterFrame()
    scopes("topLevel") = IntValue(1)
    assert(scopes("topLevel") === IntValue(1))

    scopes.enterScope()
    scopes("topLevelScope") = IntValue(2)
    assert(scopes("topLevel") === IntValue(1))
    assert(scopes("topLevelScope") === IntValue(2))

    scopes.enterFrame()
    scopes("anotherLevel") = IntValue(3)
    assert(scopes("anotherLevel") === IntValue(3))

    scopes.enterScope()
    scopes("anotherLevelScope") = IntValue(4)
    assert(scopes("anotherLevel") === IntValue(3))
    assert(scopes("anotherLevelScope") === IntValue(4))
    assertVariableNotDefined(scopes, "topLevel")
    assertVariableNotDefined(scopes, "topLevelScope")
    scopes.exitScope()

    assertVariableNotDefined(scopes, "anotherLevelScope")
    scopes.exitScope()

    // Should now be back in the first frame
    assertVariableNotDefined(scopes, "anotherLevel")
    assert(scopes("topLevel") === IntValue(1))
    assert(scopes("topLevelScope") === IntValue(2))

    scopes.exitScope()
    assertVariableNotDefined(scopes, "topLevelScope")

    scopes.exitScope()

    // Now not in a frame
    assertVariableNotDefined(scopes, "topLevel")
  }

  test("Ghosting happens as expected") {
    val scopes = new Scopes()
    scopes("a") = IntValue(1)
    scopes("b") = IntValue(-1)
    assert(scopes("a") === IntValue(1))
    assert(scopes("b") === IntValue(-1))

    scopes.enterFrame()
    scopes.setFrameVariable("a", IntValue(2)) // Ghosts the original "a"
    assert(scopes("a") === IntValue(2))

    scopes.enterFrame()
    scopes.setFrameVariable("a", IntValue(3)) // Ghosts the global "a" *and* is distinct from other frame's
    assert(scopes("a") === IntValue(3))
    scopes("b") = IntValue(-2) // Editing a global inside a frame
    scopes("a") = IntValue(10) // Changes only the local frame variable
    assert(scopes("a") === IntValue(10))
    scopes.exitScope()

    // Back in the first frame
    assert(scopes("a") === IntValue(2))
    scopes.exitScope()

    // Not in a frame anymore
    assert(scopes("a") === IntValue(1))
    assert(scopes("b") === IntValue(-2))
  }

  def assertVariableNotDefined(scopes: Scopes, variableName: String): Unit = {
    val intercepted = intercept[Exception] {
      scopes(variableName)
    }
    assert(intercepted.getMessage === s"Variable $variableName is not defined")
  }
}