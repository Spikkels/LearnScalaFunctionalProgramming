object Excercise_Chapter_2 extends App{

  //Exercise 2.1
  def getFibonacci( n: Int ): Int = {
    def go (n: Int, current: Int, previous: Int): Int = {
      if (n == 0) previous
      else go(n - 1 , current + previous, current)
    }

    go (n, 1, 0)
  }
  println("Exercise 2.1")
  println(getFibonacci(5))


  //Exercise 2.2
  def isSorted[A](as: Array[A], f: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n >= as.length-1) true
      else if (f(as(n), as(n+1))) false
      else loop(n+1)

    loop(0)
  }

  println("Exercise 2.1")
  println(isSorted(Array(9,6,3), (x: Int,y: Int) => x > y))
  println(isSorted(Array(10,15,20), (x: Int,y: Int) => x > y))
  println(isSorted(Array("A","B","C"), (x: String,y: String) => x > y))
  println(isSorted(Array("D","B","C"), (x: String,y: String) => x > y))

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    b => f(a,b)

  //Exercise 2.3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
     a => b => f(a,b)

  //Exercise 2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a,b) => f(a)(b)
  }

  //Exercise 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }

}

