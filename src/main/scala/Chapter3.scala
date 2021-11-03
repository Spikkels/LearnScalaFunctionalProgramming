import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
//  def test(ints: List[Int]): Int = ints match {
//    case _ => 42
//    case Cons(h, _) => h
//    case Cons(x,t)=> x + t : List[Int]
//    case Nil => 42
//  }


  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product (ds: List[Double]): Double = ds match {
    case Nil => 1.0
//    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))


  def foldRight[A,B] (as: List[A], z: B) (f: (A, B) => B) : B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2 (ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

}

object Chapter3 extends App{
//  println(List.test(List(1,2,3,4)))
  println(List.sum(List(1,2,3,4)))

  def append[A](a1 : List[A], a2 : List[A]) : List[A] = {
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }
  }
  println(append(List(1,2,3,4),List(5,6,7,8)))

  def recursiveLength(list: List[Int]): Int = list match {
    case Nil => 0
    case Cons(_, tail) =>  1 + recursiveLength(tail)
  }

  println(recursiveLength(List(1,2,3)))

  @tailrec
  def tailRecursiveLength(list: List[Long], accumulator : Long = 0): Long = list match {
    case Nil => accumulator
    case Cons(_, tail) => tailRecursiveLength(tail, accumulator + 1)
  }

  println(tailRecursiveLength(List()))
}
