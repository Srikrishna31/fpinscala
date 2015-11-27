package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def toList() : List[A] =
    foldRight(Nil: List[A])((a, z ) => a :: z)

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] =
  this match
  {
    case Empty => empty
    case Cons(h, t) => if (n > 1) cons(h(), t().take(n - 1)) else empty
    case Cons(h, _) => if (n == 1) cons(h(), empty) else empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] =
  this match
  {
    case Cons(h, t) => if (n > 0) t().drop(n - 1) else empty
    case _ => this
  }

  final def takeWhile(p: A => Boolean): Stream[A] =
  this match
  {
    case Empty => empty
    case Cons(h, t) => if (p(h())) cons(h(), t().takeWhile(p)) else empty
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, z) => p(a) && z)

  def takeWhileWithFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Empty : Stream[A])((a, z) => if (p(a)) cons(a, z) else empty)

  def headOption: Option[A] =
    foldRight(None : Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B) : Stream[B] =
    foldRight(Empty: Stream[B])((a, z) => cons(f(a), z))

  def filter(p: A => Boolean) : Stream[A] =
    foldRight(Empty : Stream[A])((a, z) => if (p(a)) cons(a, z) else z)

  def append[B>:A](b : => Stream[B]) : Stream[B] =
    foldRight(b)((a, z) => cons(a, z) )

  def flatMap[B](f: A => Stream[B]) : Stream[B] =
    foldRight(Empty: Stream[B])((a, z) => f(a) append z)

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match
    {
      case Some((a,s)) => cons(a, unfold(s)(f))
      case None => empty
    }

  def startsWith[B](s: Stream[B]): Boolean =
    zipAllWithUnfold(s).takeWhile(_._2.isEmpty).forAll(
      {
        case (Some(h1), Some(h2)) => h1 == h2
        case (_, Some(_)) => false
        case (Some(_), _) => false
        case (None, None) => false
      })

  def onesUsingUnfold() : Stream[Int] =
    unfold(1)(_ => Some(1, 1))

  def constantUsingUnfold[A](a: A) : Stream[A] =
    unfold(a)(_ => Some(a, a))

  def fromUsingUnfold(n : Int): Stream[Int] =
    unfold(n)(n => Some(n, n + 1))

  def fibsUsingUnfold(): Stream[Int] =
    unfold((0,1)){case (f1, f2) => Some((f1, (f2, f1 + f2))) }

  def mapWithUnfold[B](f: A => B) : Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeWithUnfold(n: Int) : Stream[A] =
    unfold((n, this)) {
      case (n,Cons(h, t)) => if (n > 0) Some((h(), (n - 1, t()))) else None
      case (_, empty) => None
    }

  def takeWhileWithUnfold(p: A => Boolean) : Stream[A] =
    unfold(this)
  {
    case Cons(h,t) => if(p(h())) Some(h(), t()) else None
    case empty => None
  }

  def zipWithWithUnfold[B, C](s2: Stream[B])(f: (A,B) => C) : Stream[C] =
    unfold(this, s2)
  {
    case (Cons(h1,t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case (_, Empty) => None
    case (empty, _) => None
  }

  def zipAllWithUnfold[B](s2: Stream[B]) : Stream[(Option[A], Option[B])] =
    unfold(this, s2)
  {
    case (Cons(h1,t1), Cons(h2,t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h, t), _) => Some((Some(h()), None), (t(), Empty))
    case (_, Cons(h, t)) => Some((None, Some(h())), (Empty, t()))
    case(Empty, Empty) => None
  }

  def tails(): Stream[Stream[A]] =
    unfold(this){
      case Empty => None
      case s => Some((s, s drop 1))
    }

  def hasSubsequence[B](s: Stream[B]) : Boolean =
    tails exists (_ startsWith s)

  //This is a complicated implementation that needs to be looked back later.
  def scanRight[S](z: S)(f:(A, => S) => S) : Stream[S] =
    foldRight((z, Stream(z)))((a, p0) =>
      {
        // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
        lazy val p1 = p0
        val b2 = f(a, p1._1)
        (b2, cons(b2, p1._2))
      })._2
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A) : Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs() : Stream[Int] =
  {
    def loop(n1 : Int, n2 : Int) : Stream[Int] = cons(n1, loop(n2, n1 + n2))

    loop(0, 1)
  }

}

object StreamMethods
{
  def main(args: Array[String]) : Unit =
  {
    println(Stream(1,2,3,4).map(_ + 10).filter(_ % 2 == 0).toList)

    lazy val ones: Stream[Int] = Stream cons(1, ones)

    println(ones.take(5).toList)

    println(ones.exists(_ % 2 != 0))

    println(ones.map(_ + 1).exists(_ % 2 == 0))

    println(ones.takeWhile(_ == 1))

    println(ones.forAll(_ != 1))
  }
}
