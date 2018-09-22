import scala.util.parsing.combinator._

case class Identifier(name: String) {
  override def toString: String = name
}

case class StringLiteral(x: String) extends Primary {
  override def evaluate: ValueInstance = StringValue(x)
  override def toString: String = s""""$x""""
}

case class IntLiteral(x: Int) extends Primary {
  override def evaluate: ValueInstance = IntValue(x)
  override def toString: String = s"$x"
}

case class AdditionEvaluable(x: Evaluable, y: Evaluable) extends Expression {
  override def evaluate: ValueInstance = x.evaluate + y.evaluate
  override def toString: String = s"$x + $y"
}

case class SubtractionEvaluable(x: Evaluable, y: Evaluable) extends Expression {
  override def evaluate: ValueInstance = x.evaluate - y.evaluate
  override def toString: String = s"$x - $y"
}

case class Braces(expression: Expression) extends Primary {
  override def evaluate: ValueInstance = expression.evaluate
  override def toString: String = s"($expression)"
}

case class MultiplicationTerm(x: Evaluable, y: Evaluable) extends Term {
  override def evaluate: ValueInstance = x.evaluate * y.evaluate
  override def toString: String = s"($x * $y)"
}

case class DivisionTerm(x: Evaluable, y: Evaluable) extends Term {
  override def evaluate: ValueInstance = x.evaluate / y.evaluate
  override def toString: String = s"($x / $y)"
}

object BaseTypes extends Enumeration {
  val integer, string = Value
}

abstract class ValueInstance {
  def +(that: ValueInstance): ValueInstance
  def -(that: ValueInstance): ValueInstance
  def *(that: ValueInstance): ValueInstance
  def /(that: ValueInstance): ValueInstance
}

case class IntValue(value: Int) extends ValueInstance {
  def +(that: ValueInstance): ValueInstance = {
    that match {
      case intThat: IntValue => IntValue(this.value + intThat.value)
      case _ => throw new Exception("Not supported type")
    }
  }

  def -(that: ValueInstance): ValueInstance = {
    that match {
      case intThat: IntValue => IntValue(this.value - intThat.value)
      case _ => throw new Exception("Not supported type")
    }
  }

  def *(that: ValueInstance): ValueInstance = {
    that match {
      case intThat: IntValue => IntValue(this.value * intThat.value)
      case _ => throw new Exception("Not supported type")
    }
  }

  def /(that: ValueInstance): ValueInstance = {
    that match {
      case intThat: IntValue => IntValue(this.value / intThat.value)
      case _ => throw new Exception("Not supported type")
    }
  }

  override def toString: String = this.value.toString
}

case class StringValue(value: String) extends ValueInstance {
  def +(that: ValueInstance): ValueInstance = {
    that match {
      case stringThat: StringValue => StringValue(this.value + stringThat.value)
      case _ => throw new Exception("Not supported type")
    }
  }

  def -(that: ValueInstance): ValueInstance = {
    throw new Exception("Not supported type")
  }

  def *(that: ValueInstance): ValueInstance = {
    that match {
      case intThat: IntValue => StringValue(this.value * intThat.value)
      case _ => throw new Exception("Not supported type")
    }
  }

  def /(that: ValueInstance): ValueInstance = {
    throw new Exception("Not supported type")
  }

  override def toString: String = s""""${this.value}""""
}

abstract class Evaluable {
  def evaluate: ValueInstance
}
abstract class Expression extends Evaluable
abstract class Term extends Expression
abstract class Primary extends Term

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