package fpinscala.datastructures

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
      case Nil => Nil
      case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match
  {
    case Nil => List(h)
    case Cons(_, xs) => Cons(h, xs)
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
  l match
  {
    case Nil => Nil
    case Cons(_, xs) => if (n > 0) drop(xs, n - 1) else xs
  }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
  l match
  {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else xs
  }


  def init[A](l: List[A]): List[A] =
  {
    @annotation.tailrec
    def loop(curr: List[A], accum: List[A]) : List[A] =
    curr match
    {
      case Nil => accum
      case Cons(x, Nil) => accum
      case Cons(x, xs) => loop(xs, append(accum, List(x)) )
    }

    loop(l, Nil)
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_,acc) => acc + 1 )

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
  l match
  {
    case Nil => z
    case Cons(x, xs) =>foldLeft(xs, f(z,x))(f)
  }

  def reverse[A](l : List[A]) : List[A] =
      foldRight(l, Nil: List[A])((x, xs) => List.append(xs, List(x)))

  def append1[A](l1 : List[A], l2: List[A]):List[A] =
      foldRight(l1, l2)((x, xs) => Cons(x, xs))

  def concat[A](l: List[List[A]]) : List[A] =
      foldRight(l, Nil: List[A])((x, ys) => foldRight(x, ys)((a, as) => Cons(a, as)))

  //Not sure of the correctness of the implementation.
  def foldLeft1[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, z)((x, z) => f(z,x))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil : List[B])((x, xs) => Cons(f(x), xs))

  def filter[A](l: List[A])(f: A => Boolean) : List[A] =
    foldRight(l, Nil: List[A])((x, ys) => if (f(x)) Cons(x, ys) else ys)

  def flatMap[A,B](as: List[A])(f: A => List[B]) : List[B] =
    foldRight(as, Nil: List[B])((x, ys) => append(f(x), ys))

  def filter1[A](l: List[A])(f: A => Boolean) : List[A] =
    flatMap(l)((x) => if (f(x)) List(x) else Nil)

  def zipWith[A, B, C](xs: List[A], ys: List[B])(f: (A, B) => C) : List[C] =
    (xs, ys) match
    {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(x, xrs), Cons(y, yrs)) => Cons(f(x, y), zipWith(xrs, yrs)(f))
    }

  @annotation.tailrec
  def hasSubsequence[A](xs: List[A], sub: List[A]) : Boolean =
    (xs, sub) match
    {
      case (Nil, _) => false
      case (_, Nil) => false
      case (Cons(y, ys), Cons(s, ss)) => if (y == s)
                                            if(ss == Nil)
                                              true
                                            else
                                              hasSubsequence(ys, ss)
                                         else
                                            hasSubsequence(ys, sub)
    }
}

object ListMethods
{
  def testListReverse():Unit =
  {
    val l = List(1,2,3,4)
    println("Reverse of list : " + l.toString() + "is : " + List.reverse(l))
  }

  def testFoldLeft() : Unit =
  {
    val l = List(1,2,3,4,5)
    println("Sum of : " + l.toString() + "is: " + List.foldLeft(l, 0)((acc, x) => acc + x))
  }

  def testAppend1(): Unit =
  {
    val l1 = List(1,2,3,4)
    val l2 = List(5,6,7,8)

    println("Result of append of " + l1.toString() + " and " + l2.toString() + " is: " + List.append1(l1, l2))
  }

  def testConcat(): Unit =
  {
    val l = List(List(1,2,3,4),
                 List(5,6,7,8),
                 List(9,10,11,12))

    println("Result of concat of " + l.toString() + " is: " + List.concat(l))
  }

  def testMap(): Unit =
  {
    val l = List(1,2,3,4)

    println("Result of squaring elements of list: " + l.toString() + " is " + List.map(l)((x) => x * x))
  }

  def testFilter() : Unit =
  {
    val l = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    println("Result of filtering odd numbers out of list: " + l.toString() + " is " + List.filter(l)((x) => x % 2 == 0 ))
  }

  def testFilter1() : Unit =
  {
    val l = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    println("Result of filtering odd numbers out of list: " + l.toString() + " is " + List.filter1(l)((x) => x % 2 == 0 ))
  }

  def testZipWith() : Unit =
  {
    val l1 = List(1, 2, 3)
    val l2 = List(4, 5, 6)

    println("Result of adding corresponding elements in lists: " +
            l1.toString() + " and " + l2.toString() + " is : " +
            List.zipWith(l1, l2)((_ + _)))
  }

  def testSubSequence(): Unit =
  {
    val l1 = List(1,2,3,4)
    val l2 = List(2,3)
    val l3 = List(5,6)
    println("Result of hasSubSequence of " +
            l2.toString() +
            " in " +
            l1.toString() +
            " is " + List.hasSubsequence(l1, l2).toString())

    println("Result of hasSubSequence of " +
      l3.toString() +
      " in " +
      l1.toString() +
      " is " + List.hasSubsequence(l1, l3).toString())

  }
  def main(args: Array[String]) : Unit =
  {
    testListReverse()
    testFoldLeft()
    testAppend1()
    testConcat()
    testMap()
    testFilter()
    testFilter1()
    testZipWith()
    testSubSequence()
  }
}
