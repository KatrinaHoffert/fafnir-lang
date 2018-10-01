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
}
