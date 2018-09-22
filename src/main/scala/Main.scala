import scala.util.parsing.combinator._

case class AdditionExpression(x: Int, y: Int) {
  def evaluate: Int = x + y
  override def toString = s"$x + $y"
}

class SimpleParser extends RegexParsers {
  def identifier: Parser[String]   = """[A-Za-z_][A-Za-z_0-9]+""".r ^^ { _.toString }
  def int: Parser[Int]    = """[0-9]+""".r ^^ { _.toInt }
  def addition: Parser[AdditionExpression] = int ~ int ^^ { case x ~ y => AdditionExpression(x, y) }
}

object TestSimpleParser extends SimpleParser {
  def main(args: Array[String]): Unit = {
    parse(addition, "2 3") match {
      case Success(matched, _) => println(matched.evaluate)
      case Failure(msg, _) => println(s"FAILURE: $msg")
      case Error(msg, _) => println(s"ERROR: $msg")
    }
  }
}