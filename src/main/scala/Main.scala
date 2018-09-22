import scala.util.parsing.combinator._

case class Identifier(name: String) {
  override def toString: String = name
}

case class IntExpression(x: Int) extends Expression {
  override def evaluate: Int = x
  override def toString: String = s"$x"
}

case class AdditionExpression(x: Expression, y: Expression) extends Expression {
  override def evaluate: Int = x.evaluate + y.evaluate
  override def toString: String = s"$x + $y"
}

case class SubtractionExpression(x: Expression, y: Expression) extends Expression {
  override def evaluate: Int = x.evaluate - y.evaluate
  override def toString: String = s"$x - $y"
}

abstract class Expression {
  def evaluate: Int
}

class SimpleParser extends RegexParsers {
  def identifier: Parser[Identifier] = """[A-Za-z_][A-Za-z_0-9]+""".r ^^ { name => Identifier(name) }
  def expression: Parser[Expression] = addition | subtraction | int
  def int: Parser[Expression] = """[0-9]+""".r ^^ { x => IntExpression(x.toInt) }
  def addition: Parser[Expression] = int ~ "+" ~ expression ^^ { case x ~ _ ~ y => AdditionExpression(x, y) }
  def subtraction: Parser[Expression] = int ~ "-" ~ expression ^^ { case x ~ _ ~ y => SubtractionExpression(x, y) }
}

object TestSimpleParser extends SimpleParser {
  def main(args: Array[String]): Unit = {
    parse(expression, "4 + 3 - 1") match {
      case Success(matched, _) => println(matched.evaluate)
      case Failure(msg, _) => println(s"FAILURE: $msg")
      case Error(msg, _) => println(s"ERROR: $msg")
    }
  }
}