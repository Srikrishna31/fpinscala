package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree
{
  def size[A](root: Tree[A]): Int =
    root match
    {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }

  def maximum(root: Tree[Int]) : Int =
  root match
  {
      case Leaf(v) => v
      case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth(root: Tree[Int]) : Int =
  root match
  {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  def map[A,B](root: Tree[A])(f: A => B) : Tree[B] =
  root match
  {
    case Leaf(v) => Leaf(f(v))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A,B](root: Tree[A])(l: A => B)(b:(B,B) => B) : B =
  root match
  {
    case Leaf(v) => l(v)
    case Branch(left, right) => b(fold(left)(l)(b), fold(right)(l)(b))
  }

  def size1[A](root: Tree[A]) : Int =
    fold(root)((a:A) => 1)(1 + _ + _)

  def maximum1(root: Tree[Int]) :Int =
    fold(root)((a) => a)(_ max _)

  def depth1[A](root: Tree[A]) : Int =
    fold(root)(a => 0)((d1, d2) => 1 + d1 max d2)
}
object TreeMethods
{
  def testSize() : Unit =
  {

  }

  def main(args: Array[String]) : Unit =
  {
    testSize()
  }
}
