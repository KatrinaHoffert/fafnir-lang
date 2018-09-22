import scala.util.parsing.combinator._

class SimpleParser extends RegexParsers {
  def identifier: Parser[Identifier] = """[A-Za-z_][A-Za-z_0-9]+""".r ^^ { name => Identifier(name) }
  def intLiteral: Parser[Primary] = """[0-9]+""".r ^^ { x => IntLiteral(x.toInt) }
  def stringLiteral: Parser[Primary] = """"[^"\\]*(\\.[^"\\]*)*"""".r ^^ { x =>
    val stringContents = x.slice(1, x.length - 1)
    StringLiteral(stringContents)
  }
  def primary: Parser[Primary] = braces | intLiteral | stringLiteral
  def term: Parser[Term] = multiplication | division | primary
  def expression: Parser[Expression] = addition | subtraction | term
  def braces: Parser[Primary] = "(" ~ expression ~ ")" ^^ { case _ ~ x ~ _ => Braces(x)}
  def multiplication: Parser[Term] = primary ~ "*" ~ term ^^ { case x ~ _ ~ y => MultiplicationTerm(x, y) }
  def division: Parser[Term] = primary ~ "/" ~ term ^^ { case x ~ _ ~ y => DivisionTerm(x, y) }
  def addition: Parser[Expression] = term ~ "+" ~ expression ^^ { case x ~ _ ~ y => AdditionEvaluable(x, y) }
  def subtraction: Parser[Expression] = term ~ "-" ~ expression ^^ { case x ~ _ ~ y => SubtractionEvaluable(x, y) }
}

object TestSimpleParser extends SimpleParser {
  def main(args: Array[String]): Unit = {
    val expressions = Seq(
      "4 + (3 - 1) * 2",
      "\"hello\" * 2",
      "\"hello\" + \" \" + \"world\"",
      "5 / 2 - 2",
    )

    for(expr <- expressions) {
      parse(expression, expr) match {
        case Success(matched, _) => println(s"$expr = ${matched.evaluate}")
        case Failure(msg, _) => println(s"FAILURE: $msg")
        case Error(msg, _) => println(s"ERROR: $msg")
      }
    }
  }
}