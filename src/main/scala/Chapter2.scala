object MyModule {

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)
    go(n, 1)
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
    }

    def formatResult(name: String, n: Int, f: Int => Int) = {
      val msg = "The %s of %d is %d."
      msg.format(name, n, f(n))
    }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop (n + 1)
    }

    loop(0)
  }

  def findFirst1[A](as: Array[A])(p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop (n + 1)
    }

    loop(0)
  }

  def test(x: Int, y: Int): Boolean = {
    x > y
  }

  def test1(x: String, y: String): Boolean = {
    x > y
  }

  val lessThan = new ((Int, Int) => Boolean) {
    def apply(a: Int, b: Int) = a == b
  }


  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatResult("absolute value", -42, abs))

    println(findFirst(Array(7, 9, 13), (x:Int) => x == 9))
    println(findFirst1(Array(7, 9, 13))(x => x == 9))

   println(lessThan.apply(5,5))
    println(test1("b","a"))

  }
  }







