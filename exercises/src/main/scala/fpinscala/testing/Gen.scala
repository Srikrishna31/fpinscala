package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
  def check: Boolean = ???

  def &&(p: Prop) : Prop = ???
    //if (this.check) p.check else false
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

case class InGen[A](sample: State[RNG, A])
{
  def choose (start: Int, stopExclusive: Int) : InGen[Int] =
    InGen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
  def boolean : Gen[Boolean] = ???
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = ???
}

trait Gen[A] {

  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}

trait SGen[+A] {

}

