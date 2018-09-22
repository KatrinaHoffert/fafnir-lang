import scala.util.parsing.combinator._

case class Identifier(name: String) {
  override def toString: String = name
}

case class IntEvaluable(x: Int) extends Primary {
  override def evaluate: Int = x
  override def toString: String = s"$x"
}

case class AdditionEvaluable(x: Evaluable, y: Evaluable) extends Expression {
  override def evaluate: Int = x.evaluate + y.evaluate
  override def toString: String = s"$x + $y"
}

case class SubtractionEvaluable(x: Evaluable, y: Evaluable) extends Expression {
  override def evaluate: Int = x.evaluate - y.evaluate
  override def toString: String = s"$x - $y"
}

case class Braces(expression: Expression) extends Primary {
  override def evaluate: Int = expression.evaluate
  override def toString: String = s"($expression)"
}

case class MultiplicationTerm(x: Evaluable, y: Evaluable) extends Term {
  override def evaluate: Int = x.evaluate * y.evaluate
  override def toString: String = s"($x * $y)"
}

case class DivisionTerm(x: Evaluable, y: Evaluable) extends Term {
  override def evaluate: Int = x.evaluate / y.evaluate
  override def toString: String = s"($x / $y)"
}

abstract class Evaluable {
  def evaluate: Int
}
abstract class Expression extends Evaluable
abstract class Term extends Expression
abstract class Primary extends Term

class SimpleParser extends RegexParsers {
  def identifier: Parser[Identifier] = """[A-Za-z_][A-Za-z_0-9]+""".r ^^ { name => Identifier(name) }
  def primary: Parser[Primary] = braces | int
  def term: Parser[Term] = multiplication | division | primary
  def expression: Parser[Expression] = addition | subtraction | term
  def int: Parser[Primary] = """[0-9]+""".r ^^ { x => IntEvaluable(x.toInt) }
  def braces: Parser[Primary] = "(" ~ expression ~ ")" ^^ { case _ ~ x ~ _ => Braces(x)}
  def multiplication: Parser[Term] = primary ~ "*" ~ term ^^ { case x ~ _ ~ y => MultiplicationTerm(x, y) }
  def division: Parser[Term] = primary ~ "/" ~ term ^^ { case x ~ _ ~ y => DivisionTerm(x, y) }
  def addition: Parser[Expression] = term ~ "+" ~ expression ^^ { case x ~ _ ~ y => AdditionEvaluable(x, y) }
  def subtraction: Parser[Expression] = term ~ "-" ~ expression ^^ { case x ~ _ ~ y => SubtractionEvaluable(x, y) }
}

object TestSimpleParser extends SimpleParser {
  def main(args: Array[String]): Unit = {
    parse(expression, "4 + (3 - 1) * 2") match {
      case Success(matched, _) => println(matched.toString)
      case Failure(msg, _) => println(s"FAILURE: $msg")
      case Error(msg, _) => println(s"ERROR: $msg")
    }
  }
}