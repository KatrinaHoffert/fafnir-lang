object Constants {
  /**
    * A unit of indentation. Used for pretty-printing.
    */
  val indentation = "  "

  /**
    * Essentially prevents infinite recursion, since our stack is actually on the heap and doesn't have a typical stack
    * limit as a result (but in practice, this limit simply prevents infinite recursion, which otherwise would eat
    * up all our memory).
    */
  val maxFrames = 250

  val builtinFunctionSignatures = Map(
    "Int$__add__Int" -> ("Int", List(("y", "Int"))),
    "Int$__sub__Int" -> ("Int", List(("y", "Int"))),
    "Int$__mult__Int" -> ("Int", List(("y", "Int"))),
    "Int$__mult__String" -> ("String", List(("y", "String"))),
    "Int$__div__Int" -> ("Int", List(("y", "Int"))),
    "String$__add__String" -> ("String", List(("y", "String"))),
    "String$__mult__Int" -> ("String", List(("y", "Int"))),
  )
}
