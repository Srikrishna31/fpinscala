package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match
  {
    case Nil => Nil  //TODO: Throw an exception here.
    case Cons(x, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match
  {
    case Nil => List(h)
    case _ => Cons(h, l)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match
  {
    case Nil => Nil
    case Cons(x, xs) => if (n == 0) xs else drop(xs, n - 1)
  }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match
  {
    case Cons(x, xs) if(f(x)) =>  dropWhile(xs, f)
    case _ => l

  }

  def init[A](l: List[A]): List[A] = l match
  {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, z) => 1 + z)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match
  {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sumUsingFoldLeft(l: List[Int]) : Int = foldLeft(l, 0)(_+_)

  def productUsingFoldLeft(l: List[Int]): Int = foldLeft(l,1)(_*_)

  def lengthUsingFoldLeft(l: List[Int]) : Int = foldLeft(l, 0)((z,_)=> 1 + z)

  def reverseUsingFoldLeft[A](l: List[A]) : List[A] = foldLeft(l, List[A]())((z, a) => Cons(a, z))

  def foldLeftUsingFoldRight[A,B](l: List[A], z: B)(f:(B,A) => B) : B = foldRight(l, z)((a, z) => f(z, a))

  def foldRightUsingFoldLeft[A,B](l: List[A], z: B)(f:(A, B) => B) : B = foldLeft(l, z)((z, a) => f(a, z))

  def appendUsingFoldRight[A](l1: List[A], l2: List[A]) : List[A] = foldRight(l1, l2)(Cons(_,_))

  def concatenate[A](l: List[List[A]]) : List[A] = foldLeft(l, List[A]())((z, a) => foldRight(a, z)(Cons(_,_)))


  def addOne(l: List[Int]) : List[Int] = l match
  {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, addOne(xs))
  }

  def doubleToString(l: List[Double]) : List[String] = l match
  {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString, doubleToString(xs))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, List[B]())((a, z) => Cons(f(a), z))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldLeft(as, List[A]())((z, a) => if (f(a)) Cons(a, z) else z)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = foldLeft(as, List[B]())((z, a) => foldLeft(f(a), z)((z1, a1) => Cons(a1, z1)))

  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else Nil)

  def combine(l1: List[Int], l2: List[Int])(f: (Int, Int) => Int) : List[Int] = (l1, l2) match
  {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), combine(t1, t2)(f))
  }

  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C) : List[C] = (l1, l2) match
  {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  def hasSubSequence[A](sup: List[A], sub: List[A]) : Boolean =
  {
      @tailrec
      def internal[A](as: List[A], bs: List[A]) : Boolean = (as, bs) match
      {
        case (_, Nil) => true
        case (Nil, _) => false
        case (Cons(h1, t1), Cons(h2, t2)) => if (h1 == h2) internal(t1, t2) else internal(t1, sub)
      }

    internal(sup, sub)
  }
}
