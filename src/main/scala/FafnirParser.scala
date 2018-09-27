import scala.util.parsing.combinator._

class FafnirParser extends RegexParsers {
  def identifier: Parser[Identifier] = """[A-Za-z_][A-Za-z_0-9]+""".r ^^ { name => Identifier(name) }

  def intLiteral: Parser[Primary] = """-?[0-9]+""".r ^^ { x => IntLiteral(x.toInt) }

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
