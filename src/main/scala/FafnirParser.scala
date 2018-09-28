import scala.util.parsing.combinator._

class FafnirParser extends RegexParsers {
  // Literal components
  def identifier: Parser[Identifier] = """[A-Za-z_][A-Za-z_0-9]*""".r ^^ { name => Identifier(name) }

  def intLiteral: Parser[Primary] = """-?[0-9]+""".r ^^ { x => IntLiteral(x.toInt) }

  def stringLiteral: Parser[Primary] = """"[^"\\]*(\\.[^"\\]*)*"""".r ^^ { x =>
    val stringContents = x.slice(1, x.length - 1)
    StringLiteral(stringContents)
  }

  // Expression evaluation components
  def expression: Parser[Expression] = addition | subtraction | term

  def primary: Parser[Primary] = braces | intLiteral | stringLiteral | identifier

  def term: Parser[Term] = multiplication | division | primary

  def braces: Parser[Primary] = "(" ~ expression ~ ")" ^^ { case _ ~ x ~ _ => Braces(x) }

  def multiplication: Parser[Term] = primary ~ "*" ~ term ^^ { case x ~ _ ~ y => MultiplicationTerm(x, y) }

  def division: Parser[Term] = primary ~ "/" ~ term ^^ { case x ~ _ ~ y => DivisionTerm(x, y) }

  def addition: Parser[Expression] = term ~ "+" ~ expression ^^ { case x ~ _ ~ y => AdditionEvaluable(x, y) }

  def subtraction: Parser[Expression] = term ~ "-" ~ expression ^^ { case x ~ _ ~ y => SubtractionEvaluable(x, y) }

  // Statement components
  def statement: Parser[Statement] = assignmentStatement | ifStatement | block

  def block: Parser[Block] = "{" ~ statement.* ~ "}" ^^ { case _ ~ statements ~ _ => Block(statements) }

  def assignmentStatement: Parser[Statement] = "var".? ~ identifier ~ "=" ~ expression ~ ";" ^^ {
    case declaration ~ ident ~ _ ~ expr ~ _ => AssignmentStatement(declaration.isDefined, ident, expr)
  }

  def ifStatement: Parser[IfStatement] = "if" ~ "(" ~ expression ~ ")" ~ block ~ elifSection.* ~ elseSection.? ^^ {
    case _ ~ _ ~ expr ~ _ ~ ifBlock ~ elifSec ~ elseSec => IfStatement(expr, ifBlock, elifSec, elseSec)
  }

  def elifSection: Parser[IfStatement] = "elif" ~ "(" ~ expression ~ ")" ~ block ^^ {
    case _ ~ _ ~ expr ~ _ ~ elifBlock => IfStatement(expr, elifBlock, List.empty[IfStatement], None)
  }

  def elseSection: Parser[Block] = "else" ~ block ^^ {
    case _ ~ elseBlock => elseBlock
  }

  // Program is a list of statements
  def program: Parser[Program] = statement.+ ^^ { statements => Program(statements) }
}
