package fpinscala.parsing

import language.higherKinds
import language.implicitConversions
import fpinscala.testing._
import fpinscala.testing.Prop._
import java.util.regex._
import scala.util.matching.Regex

type Parser[+A] = Location => Result[A]

trait Parsers[ParseError, Parser[+_]] {
  //This introduces the name self to refer to this Parsers instance;
  self => //so that inner classes may refer to the methods of the trait.


  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  implicit def string(s: String): Parser[String]

  implicit def regex(r: Regex) : Parser[String]

  def char(c: Char): Parser[Char] = map(string(c.toString))(_.charAt(0))

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = if (n <= 0) succeed(List()) else map2(p, listOfN(n - 1, p))(_::_)

  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_::_) or succeed(List())//or(map2(p, many(p))(_::_),(succeed(List())))

  def many1[A](p: Parser[A]) : Parser[List[A]] = map2(p, many(p))(_::_)
  def map[A,B](a: Parser[A])(f: A => B) : Parser[B] = flatMap(a)(v => succeed(f(v)))

  def flatMap[A,B](a: Parser[A])(f: A => Parser[B]) : Parser[B]

  def  map2[A,B,C](a: Parser[A], b: => Parser[B])(f: (A,B) => C) : Parser[C] = flatMap(a)(v1 => map(b)(v2 => f(v1, v2)))
  //def map2[A,B,C](a: Parser[A], b : => Parser[B])(f: (A,B) => C) : Parser[C] = map(product(a,b))(f.tupled)//map(a ** b)(f.tupled)//map(product(a, b))(v => f(v._1, v._2))

  //Why cant we add it directly to the trait?
//  def |[B >:A](p2: Parser[B]) : Parser[B] = or(this, p2)

  def slice[A](p: Parser[A]): Parser[String]
  
  def succeed[A](a: A): Parser[A] = map(string(""))(_ => a)

  def product[A, B](p: Parser[A], q: => Parser[B]) : Parser[(A,B)] = flatMap(p)(a => map(q)(b => (a, b)))

  def label[A](msg: String)(p: Parser[A]) : Parser[A]

  def errorLocation(e: ParseError) : Location

  def errorMessage(e: ParseError) : String

  def attempt[A](p: Parser[A]) : Parser[A]

  def scope[A](msg: String)(p: Parser[A]) : Parser[A] = s =>

  /** Sequences two parsers, ignoring the result of the first. We wrap the
    * ignored half in slice, since we don't care about its result.
    */
  def skipL[B](p: Parser[Any], p2 : => Parser[B]) : Parser[B] = map2(slice(p), p2)((_, b) => b)

  /** Sequences two parsers, ignoring the result of the second. We wrap the
    * ignored half in slice, since we don't care about its result.
    */
  def skipR[B](p: Parser[B], p2: => Parser[Any]) : Parser[B] = map2(p, slice(p2))((b, _) => b)

  /** Parser which consumes zero or more whitespaces */
  def whitespace: Parser[String] = "\\s*".r

  /** Parser which consumes one or more digits */
  def digits : Parser[String] = "\\d+".r

  /** Parser which consumes reluctantly until it encounters the given string. */
  def thru(s: String) : Parser[String] = (".*?"+Pattern.quote(s)).r

  /** Unescaped quoted strings like "foo" or "bar" */
  def quoted(s: String) : Parser[String] = string("\"") *> thru("\"").map(_.dropRight(1))

  /** Unescaped or escaped string literals, like "An \n important \"Quotation\"" or "bar" */
  def escapedQuoted(s:String) : Parser[String] =
  //Partially done, and actual implementation left as an exercice.
  token(quoted.label("string literal"))

  /** Attempts `p` and strips following whitespace, usually used for the tokens of the grammar. */
  def token[A](p: Parser[A]) : Parser[A] = attempt(p) <* whitespace

  /** Zero or more repetitions of `p`, separated by 'p2', whose results are ignored. */
  def sep[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] = sep1(p, p2) or succeed(List())

  /** One or more repetitions of `p`, separated by `p2`, whose results are ignored. */
  def sep1[A](p: Parser[A], p2: Parser[Any]) : Parser[List[A]] = map2(p, many(p2 *> p))(_::_)

  /** Parses a sequence of left associative binary operators with the same prcedence. */
  def opL[A](p: Parser[A])(op: Parser[(A, A) => A]) : Parser[A] =
    map2(p, many(op ** p))((h, t) => t.foldLeft(h)((a, b) => b._1(a, b._2)))

  /** Wraps `p` in start stop delimiters. */
  def surround[A](start: Parser[Any], end: Parser[Any])(p : => Parser[A]) = start *> p <* end

  /** A parser that succeeds when given empty input. */
  def eof : Parser[String] = regex("\\z".r).label("unexpected trailing characters")


  /** The root of the grammar, expects no further input following `p`. */
  def root[A](p: Parser[A]) : Parser[A] = p <* eof

  case class ParserOps[A](p: Parser[A])
  {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2) //Use self to explicitly disambiguate reference to the or method on trait.
    def or[B >: A](p2: => Parser[B]) : Parser[B] = self.or(p, p2)
    def **[B >: A](p1: => Parser[B]): Parser[(A,B)] = self.product(p, p1)
    def product[B >: A](p1: =>  Parser[B]): Parser[(A,B)] = self.product(p, p1)
    def slice : Parser[String] = self.slice(p)
    def map[B](f: A => B) : Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]) : Parser[B] = self.flatMap(p)(f)
    def *>[B](p2: => Parser[B]) : Parser[B] = self.skipL(p, p2)
    def <*(p2: => Parser[Any]) : Parser[Any] = self.skipR(p, p2)
    def token = self.token(p)

    def label(msg: String) : Parser[A] = self.label(msg)(p)
    def scope(msg: String) : Parser[A] = self.scope(msg)(p)

    def sep(separator: Parser[Any]) = self.sep(p, separator)
    def sep1(separator: Parser[Any]) = self.sep1(p, separator)

    def as[B](b: B) : Parser[B] = self.map(self.slice(p))(_ => b)
    def opL(op: Parser[(A,A) => A]) : Parser[A] = self.opL(p)(op)
  }

  object Laws
  {
    import ParserOps._

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]) : Prop = equal(p, p.map(_))(in)

    def labelLaw[A](p: Parser[A], inputs: SGen[String]) : Prop =
      forAll(inputs ** Gen.toString) {
        case (input, msg) => run(label(msg)(p))(input) match {
          case Left(e) => errorMessage(e) == msg
          case _ => true
        }
      }
    //parser for zero or more 'a' followed by one or more 'b':
    val test = char('a').many.slice.map(_.size) ** char('b').many1.slice.map(_.size)

    //val
  }
}

object Parsers
{

}

case class Location(input: String, offset: Int = 0)
{
  def advanceBy(n: Int) : Location = copy(offset = offset + n)

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match
  {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }
}


trait Result[+A]
{
  def mapError(f: ParseError => ParseError) : Result[A] = this match
  {
    case Failure(e, b) => Failure(f(e), b)
    case _ => this
  }

  def uncommit: Result[A] = this match
  {
    case Failure(e, true) => Failure(e, false)
    case _ => this
  }

  def addCommit(isCommitted: Boolean): Result[A] = this match
  {
    case Failure(e, c) => Failure(e, c || isCommitted)
    case _ => this
  }

  def advanceSuccess(n: Int): Result[A] = this match
  {
    case Success(a, m) => Success(a, n + m)
    case _ => this
  }

}
case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

case class ParseError(stack: List[(Location, String)])
{
  //The copy method comes for free with any case class. It returns a copy of the object,
  //but with one or more attributes modified. If no new value is specified for a field,
  //it will have the same value as in the original object. Behind the scenes, this just
  //uses the ordinary mechanism for default arguments in Scala.
  def push(loc: Location, msg: String): ParseError = copy(stack = (loc,msg) :: stack)

  def label[A](s: String) : ParseError = ParseError(latestLoc.map((_,s)).toList)

  def latestLoc : Option[Location] = latest map (_._1)

  def latest : Option[(Location, String)] = stack.lastOption

  //def advanceBy(n: Int) : Location = copy(offset = offset + n)

}
//case class Location(input: String, offset: Int = 0) {
//
//  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
//  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')
//
//  def toError(msg: String): ParseError =
//    ParseError(List((this, msg)))
//
//  def advanceBy(n: Int) = copy(offset = offset+n)
//
//  /* Returns the line corresponding to this location */
//  def currentLine: String =
//    if (input.length > 1) input.lines.drop(line-1).next
//    else ""
//}
//
//case class ParseError(stack: List[(Location,String)] = List(),
//                      otherFailures: List[ParseError] = List()) {
//}




object Main extends App
{
  import fpinscala.parsing._
  import fpinscala.parsing.Parsers


//  val numA: Parser[Int] = Parsers[Char].char('a').many.map(_.size)
}