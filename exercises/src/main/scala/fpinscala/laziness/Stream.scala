package fpinscala.laziness

import Stream._

import scala.annotation.tailrec
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  //def toList() : List[A] = foldRight(Nil[A])((a, z) => a::z)

  def toList() : List[A] = this match
  {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList()
  }

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match
  {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1 ))
    case Cons(h, t) if n == 1 => cons(h(), empty)
    case _ => empty
  }


  def drop(n: Int): Stream[A] = this match
  {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match
  {
    case Cons(h, t) => { val head = h(); if (p(head)) cons(head, t().takeWhile(p)) else empty }
    case _ => empty
  }

  def takeWhileWithFoldRight(p : A => Boolean) : Stream[A] = foldRight(empty[A])((a, b) => if(p(a)) cons(a, b) else empty)
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def headOptionWithFoldRight : Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  def headOption: Option[A] = this match
  {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B) : Stream[B] = foldRight(empty[B])((a, z) => cons(f(a), z))

  def filter(p : A => Boolean) : Stream[A] = foldRight(empty[A])((a, z) => if (p(a)) cons(a, z) else z)

  def append[B >: A](b : => Stream[B]) : Stream[B] = foldRight(b)((a, z) => cons(a, z))

  def flatMap[B](f: A => Stream[B]) : Stream[B] = foldRight(empty[B])((a, z) => f(a).append(z))

  def mapWithUnfold[B](f: A => B) : Stream[B] = unfold(this){ case Cons(h,t) => Some((f(h()), t()))
                                                                    case Empty => None }

  def takeWithUnfold(n: Int) : Stream[A] = unfold((this, n))
  {
    case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
    case (Cons(h, t), 1) => Some(((h(), (empty, 0))))
    case _ => None
  }

  def takeWhileWithUnfold(p: A => Boolean) = unfold(this)
  {
    case Cons(h, t) if(p(h())) => Some((h(), t()))
    case _ => None
  }

  def zipWith[B, C](b: Stream[B])(f:(A, B) => C) : Stream[C] = unfold((this, b))
  {
    case (Empty, _) => None
    case (_, Empty) => None
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
  }

  def zipAll[B](s2: Stream[B]) : Stream[(Option[A], Option[B])] = unfold(this, s2)
  {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2()))), (t1(), t2()))
    case (_, Cons(h, t)) => Some(((None, Some(h())), (Empty, t())))
    case (Cons(h, t), _) => Some(((Some(h()), None), (t(), Empty)))
    case _ => None
  }

//  def zipWithAll[B, C](b: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] = unfold((this, b))
//  {
//    case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(f(Some(h1()), Some(h2())), ))))
//  }

  @tailrec
  final def startsWith[B >: A] (s: Stream[B]) : Boolean = (this, s) match
  {
    case (_, Empty) => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1() == h2() => t1().startsWith(t2())
    case _ => false
  }

  def startWithUnfold[B >: A](s: Stream[B]) : Boolean = zipAll(s).takeWhile(!_._2.isEmpty).forAll
  {
    case (None, None) => true
    case (Some(a), Some(b)) if(a == b) => true
    case _ => false
  }

  def tails : Stream[Stream[A]] = unfold(this)
  {
    case Empty => None
    case Cons(h,t) => Some(Cons(h, t), t())
    //case s => Some(s, s drop 1) //case Cons(h, t) => Some(Cons(h, t), t())
  } append Stream(empty) //append empty as the last element of the stream.

  def hasSubsequence[B >: A] (s: Stream[B]) : Boolean = tails exists (_ startsWith s)

  def scanRight[B](z: B)(f: (A, => B) => B) : Stream[B] = foldRight(z, Stream(z))((a, p0) =>
  {
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

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant(v: Int) : Stream[Int] = Stream.cons(v, constant(v))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs() : Stream[Int] =
  {
    def go (f0: Int, f1: Int) : Stream[Int] = Stream.cons(f0,  go(f1, f0+ f1))

    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match
  {
    case None => Empty
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
  }

  def onesWithUnfold : Stream[Int] = unfold(1)(_ => Some(1, 1))

  def constantWithUnfold(v: Int): Stream[Int] = unfold(v)(_ => Some(v, v))

  def fibsWithUnfold() : Stream[Int] = unfold((0, 1))(a => Some(a._1, (a._2, a._1 + a._2)))

  def fromWithUnfold(n: Int) : Stream[Int] = unfold(n)(n => Some(n, n + 1))
}