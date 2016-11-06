package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](root : Tree[A]) : Int = root match
  {
    case Branch(left, right) => size(left) + size(right) + 1
    case _ => 1 //Leaf
  }

  def maximum(root: Tree[Int]): Int = root match
  {
    case Branch(left, right) => maximum(left).max(maximum(right))
    case Leaf(value) => value
  }

  def depth[A](root: Tree[A]) : Int = root match
  {
    case Branch(left, right) => depth(left).max(depth(right)) + 1
    case _ => 0
  }

  def map[A, B](root: Tree[A])(f: A => B) : Tree[B] = root match
  {
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    case Leaf(value) => Leaf(f(value))
  }

  def fold[A,B](root : Tree[A])(f:A => B)(g:(B, B) => B): B = root match
  {
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    case Leaf(value) => f(value)
  }

  def sizeViaFold[A](root: Tree[A]) : Int = fold(root)(_ => 1)(_ + _ + 1)

  def maximumViaFold(root: Tree[Int]) : Int = fold(root)(a => a)(_ max _)

  def depthViaFold[A](root: Tree[A]) : Int = fold(root)(a => 0)(_ max _ + 1)

  def mapViaFold[A, B](root: Tree[A])(f: A => B) : Tree[B] = fold(root)(a => Leaf(f(a)) : Tree[B])(Branch(_, _))
}