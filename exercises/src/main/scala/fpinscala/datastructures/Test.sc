import fpinscala.datastructures.Cons
import fpinscala.datastructures.List
import fpinscala.datastructures.List._
import fpinscala.datastructures.Nil

val x = List(1,2,3,4,5) match
{
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case _ => 101
}

//Exercise 3.8
val y = foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))

//Exercise 3.16
def addOne(l: List[Int]) : List[Int] =
  foldRight(l, Nil: List[Int])((x, xs) => Cons(x + 1, xs))

val z = addOne(List(1,2,3))

//Exercise 3.17
def toStr(l: List[Double]) : List[String] =
  foldRight(l, Nil: List[String])((x, xs) => Cons(x.toString(), xs))

val aa = toStr(List(1.9, 2.0, 3.5))
