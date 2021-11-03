import Chapter3.append
import List.{foldRight, sum}

import scala.annotation.tailrec
object Chapter3_excercises extends App{

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  // Exercise 3.1  //Answer is 3


  //  Exercise 3.2
  def tail(x: List[Any]): List[Any] = x match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, xs) => xs: List[Any]
  }

  //  def tail1[A](l: List[A]): List[A] = l match {
  //    case Nil => sys.error("tail of empty list")
  //    case Cons(_,t) => t
  //  }

  println("\nExercise 3.2")
  val x = List(1,2,3,4,5) match {
    // case Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Nil))))) => 60
    case Cons (x, Cons (2, Cons(4, _))) => x
    case Nil => 42
    case Cons (x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons (h, t) => h + sum(t)
    case _ => 101
  }

  //  val y = List(1,2,3,4,5)
  //  println(y)
  println(x)


  println("\nExercise 3.2")
  println(tail(List("A","B","C","D")))

  //  Exercise 3.3
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_,t) => Cons(h,t)
  }

  println("\nExercise 3.3")
  println(setHead(List("A","B","C","D"), "WORKING"))

  //  Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(b,t) => drop(t, n-1)
    }

  println("\nExercise 3.4")
  println(drop(List("A","B","C","D"), 2))

  // Exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h,t) if f(h) => dropWhile(t, f)
      case _ => l
    }


  def dropWhile1[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
    case Cons(h,t) if f(h) => dropWhile1(t)(f)
    case _ => as
  }

  println("\nExercise 3.5")
  println(dropWhile(List("A","B","C","D"), (x: String) => x < "D"))
  println(dropWhile(List(1, 2, 3, 4), (x: Int) => x > 2))
  println(dropWhile(List(1, 2, 3, 4), (x: Int) => x == 1))
  println(dropWhile(List(4, 3, 2, 1), (x: Int) => x > 3))

  println(dropWhile1(List(1,2,3,4,5))(x => x < 4))


  //  Exercise 3.6

//  def init[A](l: List[A]): List[A] =
//    l match {
//      case Nil => sys.error("init of empty list")
//      case Cons(_,Nil) => Nil
//      case Cons(h,t) => Cons(h,init(t))
//    }

    def init[A](l: List[A]): List[A] =
      l match {
        case Nil => sys.error("init of empty list")
        case Cons(_,Nil) => Nil
        case Cons(h,t) => Cons (h, init(t))
      }

    println("\nExercise 3.6")
    println(init(List(1,2,3)))

  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => List(buf.toList: _*)
      case Cons(h,t) => buf += h; go(t)
    }
    go(l)
  }
  println(init2(List(1,2)))

  //  Exercise 3.7
  // Because if I was to make foldRight to stop when the value is
  // zero then product may stop but sum will also stop
  // and this is incorrect.


  //  Exercise 3.8
  // It displays the normal list.
  // It returns the normal list because when the case is Nil then the
  // output is Nil.
  // The function is the default Cons functions and this is why the
  // list does not change.

  def foldRight[A,B] (list: List[A], nilvalue: B) (f: (A, B) => B) : B =
    list match {
      case Nil => nilvalue
      case Cons(head, tail) => f(head, foldRight(tail, nilvalue)(f))
    }

  println("\nExercise 3.8")
  println(foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))

  //  Exercise 3.9
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_ , acc) => acc + 1)

  println("\nExercise 3.9")
  println(length(List(10,22,33,44,54)))





  //  Exercise 3.10
  //  foldRight is not recursive because it did not use @annotate.tailrec
  //  foldRight last call of the method was not recursive

//  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B

  @tailrec
  def foldLeft[A,B](list: List[A], nilValue: B)(function: (B, A) => B): B = list match {
    case Nil => nilValue
    case Cons(head,tail) => foldLeft(tail, function(nilValue,head))(function)
  }



  println("\nExercise 3.10")
  println(length(List(10,22,33,44,54)))



  //  Exercise 3.11
  def sumFoldLeft(l: List[Int]) = foldLeft(l, 0)(_ + _)
  def productFoldLeft(l: List[Int]) = foldLeft(l, 1.0)(_ * _)
  def lengthFoldLeft[A](l: List[A]): Int = foldLeft(l, 0)((acc,_) => acc + 1)


  println("\nExercise 3.11")
  println(sumFoldLeft(List(10,22,33,44,54)))
  println(productFoldLeft(List(10,22,33,44,54)))
  println(lengthFoldLeft(List(10,22,33,44,54)))



  //  Exercise 3.12
  def reverseFoldLeft[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

  println("\nExercise 3.12")
  println(reverseFoldLeft(List(10,22,33,44,54)))


  // Exercise 3.13  //Difficult and don't understands
//  def foldRightViaFoldLeft[A, B](l: List[A], z:B)(f: (A,B) => B): B =
//    foldLeft(reverse(l), z)((b,a) => f(a,b))

  def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)


  // Exercise 3.14  //Difficult and don't understands
  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)


  // Exercise 3.15
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)

  println("\nExercise 3.16")
//  println(concat(List(5,6,7,8,9))

  // Exercise 3.16
  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int])((h,t) => Cons(h+1,t))

  println("\nExercise 3.16")
  println(add1(List(5,6,7,8,9)))

  // Exercise 3.17
  def intListToString(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String])((h,t) => Cons(h.toString,t))

  println("\nExercise 3.17")
  println(intListToString(List(5,6,7,8,9)))

  // Exercise 3.18
  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((h,t) => Cons(f(h),t))

  println("\nExercise 3.18")
  println(map(List(5,6,7,8,9))(x => x < 7))


  // Exercise 3.19
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil:List[A])((h,t) => if (f(h)) Cons(h,t) else t)

  println("\nExercise 3.19")
  println(filter(List(5,6,7,8,9))(x => x < 7))

  // Exercise 3.20
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  println("\nExercise 3.20")
  println(flatMap(List(1,2,3))(i => List(i,i)))

  // Exercise 3.21
  def filterToImplementFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)


  // Exercise 3.22
  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPairwise(t1,t2))
  }

    println("\nExercise 3.22")
    println(addPairwise(List(1,2,3),List(4,5,6)))

  // Exercise 3.23
  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }

//  println("\nExercise 3.23")
//  println(zipWith(List(1,2,3),List(4,5,6))(x: Int, y: Int => y == x))


  // Exercise 3.24





}
