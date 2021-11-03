

object Chapter3_excercises_trees extends App{
  val p = ("Bob", 42)
  println(p)
  println(p._1)
  println(p._2)

  print(p match {
    case (a,b) => b
  })


  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right:Tree[A]) extends Tree[A]


  //Exercise 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left,right) => 1 + size(left) + size(right)
  }
  println("\nExercise 3.25")
  val leafLeft = Leaf(1)
  val leafRight = Leaf(5)
  val leafLeft1 = Leaf(10)
  val leafRight1 = Leaf(20)
  val branch1 = Branch(leafLeft, leafRight)
  val branch2 = Branch(branch1, leafRight1)
  println(branch2)
  println(size(branch2))


  //Exercise 3.26
  println("\nExercise 3.26")
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(left,right) => maximum(left) max maximum(right)
  }

  println(maximum(branch1))
  println(maximum(branch2))

  //Exercise 3.27
  println("\nExercise 3.27")
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  println(depth(branch1))
  println(depth(branch2))

  //Exercise 3.28
  println("\nExercise 3.28")
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

  //Exercise 3.29
  def fold[A,B](tree: Tree[A])(f: A => B)(g: (B,B) => B): B = tree match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeFold[A](t: Tree[A]): Int =
    fold(t)(a => 1)(1 + _ + _)

  def maximumFold(t: Tree[Int]): Int =
    fold(t)(a => a)(_ max _)

  def depthFold[A](t: Tree[A]): Int =
    fold(t)(a => 0)((d1,d2) => 1 + (d1 max d2))
}
