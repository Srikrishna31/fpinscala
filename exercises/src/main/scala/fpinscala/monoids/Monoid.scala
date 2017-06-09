package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps

import language.higherKinds
import scala.collection.immutable.Stream.Empty

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int]
  {
    def op(a1: Int, a2: Int) = a1 + a2
    val zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int]
  {
    def op(a1: Int, a2: Int) = a1 * a2
    val zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean]
  {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    val zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean]
  {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    val zero = true
  }

  def optionMonoid[A](f: (A, A) => A): Monoid[Option[A]] = new Monoid[Option[A]]
  {
    def op(a1: Option[A], a2: Option[A]) = a1 orElse a2
    val zero = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A]
  {
    def op(a1: A => A, a2: A => A) = a => a2(a1(a)) //could also be a => a1(a2(a))
    def zero = a => a
  }

  import fpinscala.testing._
  import Prop._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    forAll(for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z))(p => m.op(p._1, m.op(p._2, p._3)) == m.op(m.op(p._1, p._2), p._3)) &&
    forAll(gen)((a: A) => m.op(a, m.zero) == m.op(m.zero, a))

  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

//  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = foldMap(as, new Monoid[B] {
//    def op(a1: B, a2: B) = ???
//    val zero = z
//  })(a => f(a, z))

  def foldRight[A,B](as: List[A])(z: B)(f: (A, B) => B) : B = foldMap(as, endoMonoid[B])(f.curried)(z)
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = foldMap(as, endoMonoid[B])(a => b => f(b, a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
  {
    if (as.length >1)
    {
      val (left, right) = as.splitAt(as.length / 2)
      val (leftRes, rightRes) = (foldMapV(left, m)(f), foldMapV(right, m)(f))
      m.op(leftRes, rightRes)
    }
    else if (as.length == 1)
    {
      f(as.head)
    }
    else
      m.zero
  }


  def ordered(ints: IndexedSeq[Int]): Boolean =
  {
    //Our monoid tracks the minimum and maximum  elements seen so far and whether the elements are ordered.
    val monoid = new Monoid[Option[(Int, Int, Boolean)]]
    {
      def op(a1: Option[(Int, Int, Boolean)], a2: Option[(Int, Int, Boolean)]) = (a1, a2) match
      {
        case (Some((x1, y1, ordered1)), Some((x2, y2, ordered2))) => Some((x1 min x2, y1 max y2, ordered1 && ordered2 && y1 <= x2))
        case (None, _) => a2
        case (_, None) => a1
      }
      val zero = None
    }

    //The empty sequence is ordered and each element by itself is ordered.
    foldMapV(ints, monoid)(i => Some(i, i, true)).map(_._3).getOrElse(true)
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  import fpinscala.parallelism.Nonblocking._

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]]
  {
    def op(a1: Par[A], a2: Par[A]) = a1.map2(a2)(m.op)
    val zero = Par.unit(m.zero)
  }

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.parMap(v)(f).flatMap { bs => foldMapV(bs, par(m))(b => Par.async(b))}

//  val wcMonoid: Monoid[WC] = new Monoid[WC]
//  {
//    def op(a1: WC, a2: WC) = (a1, a2) match
//    {
//      case (Stub(s1), Stub(s2)) => if(s1(s1.length -1) == ' ' || s1(s1.length - 1) == ',') Part(" ", 1, s2) else Stub(s1 + s2)
//      case (Part(left1, w1, right1), Part(left2, w2, right2)) => if(left2(0) == ' ' || left2(0) == ',') Part(left1, w1 + w2 + 1, right2) else Part(left1, w1 + w2, right2)
//      case (Stub(s), Part(left, w, right)) => if (s(s.length - 1) == ' ' || s(s.length - 1) == ',') Part(left, w+1, right) else if (left(0) == ' ' || left(0) == ',') Part(left.splitAt(0)._2, w + 1, right) else Part(s + left, w, right)
//      case (Part(left, w, right), Stub(s)) => if (s(0) == ' ' || s(0) == ',') Part(left, w + 1, s.splitAt(0)._2) else Part(left, w, right + s)
//    }
//
//    val zero = Stub("")
//  }

  val wcMonoid: Monoid[WC] = new Monoid[WC]
  {
    def op(a1: WC, a2: WC) = (a1, a2) match
    {
      case (Stub(s1), Stub(s2)) => Stub(s1 + s2)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + (if((r1 + l2).isEmpty) 0 else 1) + w2, r2)
      case (Stub(s), Part(l, w, r)) => Part(s + l, w, r)
      case (Part(l, w, r), Stub(s)) => Part(l, w, r + s)
    }

    val zero = Stub("")
  }

  def count(s: String): Int =
  {
    foldMapV(s, wcMonoid)(c => if (c.isWhitespace) Part("", 0, "") else Stub(c.toString)) match
    {
      case Stub(_) => 1 //Treat the entire string as one word.
      case Part(_, w, _) => w
    }
  }

  def productMonoid[A,B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)]
  {
    def op(a1: (A, B), a2: (A, B)) = (a.op(a1._1, a2._1), b.op(a1._2, a2._2))
    val zero = (a.zero, b.zero)
  }


  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B]
  {
    def op(a1: A => B, a2: A => B) = (a: A) => B.op(a1(a), a2(a))
    def zero = _ => B.zero
  }


  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K,V]]
  {
    def op(a1: Map[K,V], a2: Map[K,V]) = (a1.keySet ++ a2.keySet).foldLeft(zero) {
      (acc, k) => acc.updated(k, V.op(a1.getOrElse(k, V.zero), a2.getOrElse(k, V.zero)))
    }

    def zero = Map[K,V]()
  }


  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A,Int](intAddition))((a: A) => Map(a -> 1))
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    sys.error("todo")

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    sys.error("todo")

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")

  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List())(_ :: _)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) = as match
  {
    case h::t => f(h, foldRight(t)(z)(f))
    case Nil => z
  }

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) = as match
  {
    case h::t => foldLeft(t)(f(z, h))(f)
    case Nil => z
  }

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = as match
  {
    case h::t => mb.op(f(h), foldMap(t)(f)(mb))
    case Nil => mb.zero
  }

}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) = as.length match
  {
    case 0 => z
    case 1 => f(as(0), z)
    case _ => f(as.head, foldRight(as.tail)(z)(f))
  }


  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) = as.length match
  {
    case 0 => z
    case 1 => f(z, as(0))
    case _ => foldLeft(as)(f(z, as.head))(f)
  }

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
  {
    if(as.length > 2)
    {
      val (left, right) = as.splitAt(as.length / 2)
      mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
    }
    else if (as.length == 1)
    {
      f(as(0))
    }
    else
      mb.zero
  }

}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) = as match
  {
    case Empty => z
    case h #:: t => f(h, foldRight(t)(z)(f))
  }


  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) = as match
  {
    case Empty => z
    case h #:: t => foldLeft(t)(f(z, h))(f)
  }

}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {

  // Notice that in `TreeFoldable.foldMap`, we don't actually use the `zero`
  // from the `Monoid`. This is because there is no empty tree.
  // This suggests that there might be a class of types that are foldable
  // with something "smaller" than a monoid, consisting only of an
  // associative `op`. That kind of object (a monoid without a `zero`) is
  // called a semigroup.

  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match
  {
    case Leaf(a) => f(a)
    case Branch(left, right) => mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
  }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) = as match
  {
    case Leaf(a) => f(z, a)
    case Branch(left, right) => {
      val leftFold = foldLeft(left)(z)(f)
      foldLeft(right)(leftFold)(f)
    }
  }

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) = as match
  {
    case Leaf(a) => f(a, z)
    case Branch(left, right) => {
      val rightFold = foldRight(right)(z)(f)
      foldRight(left)(rightFold)(f)
    }
  }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match
  {
    case None => mb.zero
    case Some(x) => f(x)
  }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) = as match
  {
    case None => z
    case Some(x) => f(z, x)
  }

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) = as match
  {
    case None => z
    case Some(x) => f(x, z)
  }

}

