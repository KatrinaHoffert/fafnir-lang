import scala.util.parsing.combinator._

class FafnirParser extends RegexParsers {
  override protected val whiteSpace: scala.util.matching.Regex = """(\s|//.*)+""".r

  // Literal components
  def identifier: Parser[Identifier] = positioned {
    """[A-Za-z_][A-Za-z_0-9]*""".r ^^ { name => Identifier(name) }
  }

  def intLiteral: Parser[Primary] = positioned {
    """-?[0-9]+""".r ^^ { x => IntLiteral(x.toInt) }
  }

  def stringLiteral: Parser[Primary] = positioned {
    """"[^"\\]*(\\.[^"\\]*)*"""".r ^^ { x =>
      // Remove the quotes
      val stringContents = x.slice(1, x.length - 1)
      StringLiteral(stringContents)
    }
  }

  // Expression evaluation components
  def expression: Parser[Expression] = positioned {
    addition | subtraction | term
  }

  def primary: Parser[Primary] = positioned {
    braces | intLiteral | stringLiteral | functionCall | identifier
  }

  def functionCall: Parser[FunctionCall] = positioned {
    identifier ~ "(" ~ repsep(expression, ",") ~ ")" ^^ {
      case funcName ~ _ ~ argExpressions ~ _ => FunctionCall(funcName, argExpressions)
    }
  }

  def term: Parser[Term] = positioned {
    multiplication | division | primary
  }

  def braces: Parser[Primary] = positioned {
    "(" ~ expression ~ ")" ^^ { case _ ~ x ~ _ => Braces(x) }
  }

  def multiplication: Parser[Term] = positioned {
    primary ~ "*" ~ term ^^ { case x ~ _ ~ y => MultiplicationTerm(x, y) }
  }

  def division: Parser[Term] = positioned {
    primary ~ "/" ~ term ^^ { case x ~ _ ~ y => DivisionTerm(x, y) }
  }

  def addition: Parser[Expression] = positioned {
    term ~ "+" ~ expression ^^ { case x ~ _ ~ y => AdditionEvaluable(x, y) }
  }

  def subtraction: Parser[Expression] = positioned {
    term ~ "-" ~ expression ^^ { case x ~ _ ~ y => SubtractionEvaluable(x, y) }
  }

  // Statement components
  def statement: Parser[Statement] = positioned {
    assignmentStatement | ifStatement | whileLoop | functionDefinition | functionCallStatement | returnStatement | block
  }

  def block: Parser[Block] = positioned {
    "{" ~ statement.* ~ "}" ^^ { case _ ~ statements ~ _ => Block(statements) }
  }

  def assignmentStatement: Parser[Statement] = positioned {
    "var".? ~ identifier ~ "=" ~ expression ~ ";" ^^ {
      case declaration ~ ident ~ _ ~ expr ~ _ => AssignmentStatement(declaration.isDefined, ident, expr)
    }
  }

  def ifStatement: Parser[Statement] = positioned {
    "if" ~ "(" ~ expression ~ ")" ~ block ~ elifSection.* ~ elseSection.? ^^ {
      case _ ~ _ ~ expr ~ _ ~ ifBlock ~ elifSec ~ elseSec => IfStatement(expr, ifBlock, elifSec, elseSec)
    }
  }

  def elifSection: Parser[IfStatement] = positioned {
    "elif" ~ "(" ~ expression ~ ")" ~ block ^^ {
      case _ ~ _ ~ expr ~ _ ~ elifBlock => IfStatement(expr, elifBlock, List.empty[IfStatement], None)
    }
  }

  def elseSection: Parser[Block] = positioned {
    "else" ~ block ^^ { case _ ~ elseBlock => elseBlock }
  }

  def whileLoop: Parser[Statement] = positioned {
    "while" ~ "(" ~ expression ~ ")" ~ block ^^ { case _ ~ _ ~ expr ~ _ ~ whileBlock => WhileLoop(expr, whileBlock) }
  }

  // Functions
  def functionDefinition: Parser[Statement] = positioned {
    "func" ~ identifier ~ argumentNameList ~ block ^^ {
      case _ ~ identifier ~ args ~ body => FunctionDeclaration(identifier, args, body)
    }
  }

  def argumentNameList: Parser[List[Identifier]] = "(" ~ repsep(identifier, ",") ~ ")" ^^ { case _ ~ args ~ _ => args }

  def functionCallStatement: Parser[Statement] = positioned {
    functionCall ~ ";" ^^ { case func ~ _ => FunctionCallStatement(func) }
  }

  def returnStatement: Parser[Statement] = positioned {
    "return" ~ expression.? ~ ";" ^^ { case _ ~ value ~ _ => ReturnStatement(value) }
  }

  // Program is a list of statements
  def program: Parser[Program] = statement.+ ^^ { statements => Program(statements) }
}
