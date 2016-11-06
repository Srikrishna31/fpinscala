package fpinscala.state

import scala.annotation.tailrec


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def mapWithFlatMap[A,B](s: Rand[A])(f: A => B) : Rand[B] = flatMap(s)(a => unit(f(a)))

  def nonNegativeInt(rng: RNG): (Int, RNG) =
  {
    val (rand, next) = rng.nextInt
    if (rand < 0)
      (-(rand + 1), next)
    else (rand, next)
  }

  def double(rng: RNG): (Double, RNG) =
  {
    val (rand, next) = nonNegativeInt(rng)
    (rand / (Int.MaxValue.toDouble + 1), next)
  }

  def doubleBetter(rng: RNG) : (Double, RNG) = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))(rng)

  def intDouble(rng: RNG): ((Int,Double), RNG) =
  {
    val (randInt, next) = rng.nextInt
    val (randDouble, next2) = double(next)
    ((randInt, randDouble), next2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) =
  {
    val ((randInt, randDouble), next) = intDouble(rng)
    ((randDouble, randInt), next)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) =
  {
    val (r1, n1) = double(rng)
    val (r2, n2) = double(n1)
    val (r3, n3) = double(n2)
    ((r1, r2, r3), n3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
  {
    @tailrec
    def go(count: Int, list: List[Int], rng: RNG) : (List[Int], RNG) =
    {
      if (count == 0)
        (list, rng)
      else
      {
        val (rand, next) = rng.nextInt
        go(count - 1, rand :: list, next)
      }
    }

    go(count, Nil, rng)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng =>
  {
    val (a1, next1) = ra(rng)
    val (a2, next2) = rb(next1)
    (f(a1, a2), next2)
  }

  def map2WithFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C) : Rand[C] = flatMap(ra)(a => map(rb)(b => f(a, b)))

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_,_))

  val randIntDouble: Rand[(Int, Double)] = map2(nonNegativeInt, double)((_,_))

  val randDoubleInt: Rand[(Double, Int)] = map2(double, nonNegativeInt)((_,_))

  val randIntDoubleBetter : Rand[(Int, Double)] = both(nonNegativeInt, double)

  val randDoubleIntBetter: Rand[(Double, Int)] = both(double, nonNegativeInt)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(List[A]()))((f, z) => map2(f, z)(_::_))

  def intsBetter(count: Int)(rng: RNG) : (List[Int], RNG) = sequence(List.fill(count)(int))(rng)

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng =>
  {
    val (a, next) = f(rng)
    g(a)(next)
  }

  def nonNegativeLessThan(n: Int) : Rand[Int] = flatMap(nonNegativeInt)
  {
    i =>
      val mod = i % n
      if ( i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  }

}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State((rng : S) =>
  {
    val (a, next) = run(rng)
    (f(a), next)
  })

  def _map[B](f: A => B) : State[S, B] = flatMap(a => State((rng: S) => (f(a), rng)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State((rng: S) =>
  {
    val (a, next) = run(rng)
    val (b, next1) = sb.run(next)
    (f(a, b), next1)
  })

  def _map2[B, C](sb: State[S, B])(f: (A, B) => C) : State[S, C] = flatMap(a => sb.map(b => (f(a, b))))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State((rng: S) =>
  {
    val (a, next) = run(rng)
    f(a).run(next)
  })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State((s: Machine) =>
    {
      val machine = inputs.foldLeft(s)((st, i) => (st, i) match
      {
        case (Machine(_, 0, _), _) => st
        case (Machine(true, _, _), Turn) => st
        case (Machine(false, _, _), Coin) => st
        case (Machine(true, candies, c), Coin) => Machine(false, candies, c + 1)
        case (Machine(false, candies, coins), Turn) => Machine(true, candies - 1, coins)
      })

      ((machine.candies, machine.coins), machine)
    })
}
