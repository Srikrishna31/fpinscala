package fpinscala.parsing

import language.higherKinds
import fpinscala.testing._

trait Parsers[ParseError, Parser[+_]] {
  self =>
  //THis introduces the name self to refer to this Parsers instance;
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  def many[A](p: Parser[A]): Parser[List[A]]

  def map[A,B](a: Parser[A])(f: A => B) : Parser[B]
  //Why cant we add it directly to the trait?
//  def |[B >:A](p2: Parser[B]) : Parser[B] = or(this, p2)

  def slice[A](p: Parser[A]): Parser[String]

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A])
  {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2) //Use self to explicitly disambiguate reference to the or method on trait.
    def or[B >: A](p2: Parser[B]) : Parser[B] = self.or(p, p2)
  }

  object Laws
  {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]) : Prop = equal(p, p.map(_))(in)
  }
}
//trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait
//
//  case class ParserOps[A](p: Parser[A]) {
//
//
//  }
//
//  object Laws {
//  }
//}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}




object Main extends App
{
  import fpinscala.parsing._
  import fpinscala.parsing.Parsers


  val numA: Parser[Int] = char('a').many.map(_.size)
}