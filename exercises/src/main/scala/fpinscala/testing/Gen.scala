package fpinscala.testing

//import fpinscala.laziness.Stream
import fpinscala.parallelism.Par
import fpinscala.state._
//import fpinscala.parallelism._
//import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

package object StreamExtend
{
  implicit class StreamExtension[A](val s: Stream[A]) extends AnyVal
  {
    def unfold[A, S](z: S)(f: S => Option[(A, S)]) : Stream[A] = f(z) match
    {
      case None => Stream.Empty
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    }
  }
}

import StreamExtend._
/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

sealed trait Result
{
  def isFalsified : Boolean
}

case object Passed extends Result
{
  def isFalsified = false
}

case object Proved extends Result
{
  def isFalsified = false
}

case class Falsified(failure: FailedCase, successes : SuccessCount) extends Result
{
  def isFalsified = true
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result)
{
  def forAll[A](as: Gen[A])(f: A => Boolean) : Prop = Prop {
    (m, n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map{
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e : Exception => Falsified(buildMsg(a, e), i)}
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def &&(p: Prop) : Prop = Prop {
    (m, n, rng) => run(m, n, rng) match
    {
      case Passed => p.run(m, n, rng)
      case x => x
    }
  }

  def ||(p: Prop) : Prop = Prop{
    (m, n, rng) => run(m, n, rng) match
    {
      case Falsified(failure, _) => p.run(m, n, rng)
      case x => x
    }
  }

  def forAll[A](g: Int => Gen[A])(f: A => Boolean) : Prop = Prop
  {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props : Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop = props.map(p => Prop { (max, _, rng) =>
        p.run(max, casesPerSize, rng)}).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  /* Just a nuance of the pimp my library idiom to add an extra function to stream */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.Empty.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception) =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage} \n" +
    s"stack trace: \n ${e.getStackTrace.mkString("\n")}"

  def check(p: => Boolean) : Prop =
  {
    lazy val result = p
    forAll(unit(()))(_ => result)
  }
  //def &&(p: Prop) : Prop = new Prop { def check = Prop.this.check && p.check }
}

object Prop {

  type SuccessCount = Int
  type TestCases = Int
  type FailedCase = String
  type MaxSize = Int
  type Result = Option[(FailedCase, SuccessCount)]

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)) : Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests: \n$msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests")
      case Proved =>
        println(s"+ OK, proved property")
    }

  def forAll[A](g: SGen[A])(f: A => Boolean) : Prop = forAll(g(_))(f)

  val smallInt = Gen.choose(-10, 10)

  val maxProp = forAll(SGen.listOf(smallInt, Gen(State(RNG.nonNegativeInt))) {
      ns =>
        val max  = ns.max
        !ns.exists(_ > max)
    }

  val sortedProp = forAll(listOf(smallInt))

  val ES: ExecutorService = Executors.newCachedThreadPool
  val p1 = Prop.forAll(Gen.unit(Par.unit(1)))(i => Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get)

}

case class Gen[A](sample: State[RNG, A])
{
  def map[A,B](f: A => B) : Gen[B] = Gen(sample.map(f(_)))
  def flatMap[A,B](f: A => Gen[B]) : Gen[B] = Gen(sample.flatMap((f(_).sample)))

  def union[A](g1: Gen[A], g2: Gen[A]) : Gen[A] = boolean.flatMap((b:Boolean) => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)) : Gen[A] =
  {
    val g1Prob = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(State(RNG.double).flatMap(v => if (v < g1Prob) g1._1.sample else g2._1.sample))
  }

  def unsized : SGen[A] =  SGen(i => this)
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State((a, _)))


  def boolean: Gen[Boolean] = Gen(State(RNG.nonNegativeInt).map(a => if (a != 0) true else false))

  def listOfN[A](n: Int, g: Gen[A]) : Gen[List[A]] = ??? //RNG.sequence(List.fill(n)(g.sample))

  def choose(start: Int, stopExclusive: Int) : Gen[Int] = Gen(State((rng: RNG) =>
  {
    val next = rng.nextInt
    ((next._1 - Int.MinValue) / (Int.MaxValue - Int.MinValue) * (stopExclusive + 1 - start), next._2)
  }))
}

case class SGen[+A](forSize: Int => Gen[A])
{
  def apply(n: Int) : Gen[A] = forSize(n)

  def map[A, B](f: A => B) : SGen[B] = SGen(forSize(_).map(f))

  def flatMap[A, B](f: A => SGen[B]) : SGen[B] = SGen(n => forSize(n).flatMap((a: A) => f(a).forSize(n)))

  //def **[B](s2: Gen[B]): SGen[(A,B)] = SGen(n => apply(n) ** s2(n))


  def listOf1[A](g: Gen[A]) : SGen[List[A]] = SGen(n => Gen.listOfN(n max 1, g)) //define a list of 1

}

object SGen
{
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => Gen.listOfN(n, g))
}
object Main extends App
{


}

