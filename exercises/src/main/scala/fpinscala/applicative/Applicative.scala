package fpinscala
package applicative

import monads.Functor
import state._
import State._
import StateUtil._ // defined at bottom of this file
import monoids._
import language.higherKinds
import language.implicitConversions

trait Applicative[F[_]] extends Functor[F] {

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(map(fa)(f.curried))(fb)

  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((f, a) => f(a))

  def unit[A](a: => A): F[A]

  def map[A,B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)

  def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A,B,C) => D): F[D] = apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A,B,C,D) => D): F[E] = apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(a => a)

//  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K, V]] = ofa.foldLeft(unit(Map[K,V]())){case (z, (k, fv)) => map2(z, fv)((m, v) => m + (k -> v))}

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] = as.foldRight(unit(List[B]()))((a, z) => map2(f(a), z)(_::_))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def factor[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa, fb)((_,_))

  def product[G[_]](G: Applicative[G]) = new Applicative[({type f[x] = (F[x], G[x])})#f]
  {
    self =>
    override def unit[A](a: => A) = (self.unit(a), G.unit(a))
    override def apply[A,B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) = (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))
  }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = new Applicative[({type f[x] = F[G[x]]})#f]
  {
    //self =>
    val self = this
    override def unit[A](a: => A) = self.unit(G.unit(a))
//    override def apply[A,B](fs: (F[A => B]))(p: F[G[A]]) = self.apply(fs)()
    override def map2[A,B, C](fga: F[G[A]], fgb: F[G[A]])(f: (A, B) => C): F[G[C]] = self.map2(fga, fgb)((ga, gb) => G.map2(ga, gb)((a, b) => f(a,b)))
  }

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] = ofa.foldLeft(unit(Map[K,V]()))((z, v) => map2(z, v._2)((m, vl) => m.updated(v._1, vl)))
}

case class Tree[+A](head: A, tail: List[Tree[A]])

//A minimal implementation of Monad must implement unit and override either flatMap or join and map
trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))

  override def map2[A,B,C](fa: F[A], fb: F[B])(f:(A,B) => C): F[C] = flatMap(fa)(a => map(fb)(b => f(a,b)))

  override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))
}

object Monad {
  def eitherMonad[E]  = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A) = Right(a)
    override def flatMap[A,B](ei: Either[E, A])(f: A => Either[E, B]) : Either[E, B] = ei match
    {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_],N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]):
    Monad[({type f[x] = F[N[x]]})#f] = new Monad[({type f[x] = F[N[x]]})#f]
  {
    override def unit[A](a: => A) : F[N[A]]= F.unit(N.unit(a))
    override def flatMap[A, B](fna: F[N[A]])(f: A => F[N[B]]): F[N[B]] = F.flatMap(fna)(na => N.map(T.traverse(na)(f))(F.join))
  }
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A,B,C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                    f: (A,B) => C): Stream[C] =
      a zip b map f.tupled
  }

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E,x]})#f] = new Applicative[({type f[x] = Validation[E, x]})#f]
  {
    override def unit[A](a: => A) = Success(a)
    override def map2[A,B, C](va1: Validation[E, A], va2: Validation[E, B])(f:(A,B) => C) : Validation[E,C] = (va1, va2) match
    {
      case (Failure(eh1, et1), Failure(eh2, et2)) => Failure(eh1, eh2+:(et1++et2))
      case (Success(a), Success(b)) => Success(f(a, b))
      case (_, e@Failure(_,_)) => e
      case (e@Failure(_,_), _) => e
    }
  }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
    }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  type Id[A] = A
  val idMonad = new Monad[Id]
  {
    def unit[A](a : => A) = a
    override def flatMap[A, B](a: A)(f: A => B) = f(a)
  }
  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_]:Applicative,A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  def map[A,B](fa: F[A])(f: A => B): F[B] = traverse[Id, A, B](fa)(f)(idMonad)

  import Applicative._

  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B,x]})#f,A,Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => (for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _  <- set(s2)
    } yield b)).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] = mapAccum(fa, (toList(fa).reverse))((_, s) => (s.head, s.tail))._1

  override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B = mapAccum(fa, z)((a, s) => ((), f(s, a)))._2

  def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])
                         (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = traverse(fa)(a => (G.map(f(a))(unit(_)), H.map(g(a))(unit(_))))

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = new Traverse[({type f[x] = F[G[x]]})#f]
  {
    self =>
    override def traverse[M[_]:Applicative, A, B](fga: F[G[A]])(f: A => M[B]) = self.traverse(fga)((ga: G[A]) => G.traverse(ga)(f))
  }
}

object Traverse {
  val listTraverse = new Traverse[List]
  {
    override def traverse[G[_], A, B](as: List[A])(f: A => G[B])(implicit G: Applicative[G]) : G[List[B]] =
      as.foldRight(G.unit(List[B]()))((z, a) => G.map2(z, f(a))((bs, b) => b::bs))
  }

  val optionTraverse = new Traverse[Option]
  {
    override def traverse[G[_], A, B](oa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] = oa match
    {
      case Some(a) => G.map(f(a))(Some(_))
      case None => G.unit(None)
    }

  }

  val treeTraverse = new Traverse[Tree]
  {
    override def traverse[G[_], A, B](ta: Tree[A])(f: A => G[B])(implicit G : Applicative[G]): G[Tree[B]] = ta match
    {
      case Tree(h, h1::t) => G.map(f(h))(Tree(_, traverse(Tree(h1, t))(f)))
    }
  }
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}
