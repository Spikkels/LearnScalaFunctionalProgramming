

sealed trait Option[+A] {

  // Exercise 4.1
  def map[B](f: A => B) : Option[B] = this match{
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]) : Option[B] = {
    map(f) getOrElse None
  }

  def orElse[B >: A] (ob: => Option[B]) : Option[B] = {
    map(Some(_)) getOrElse ob
  }

  def filter(f: A => Boolean) : Option[A] = {
    flatMap((a: A) => if(f(a)) Some(a) else None)
  }
  // Exercise 4.1 END
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


object Option {

  case class Employee(name: String, department: String, manager: Option[String])

  def lookupByName(name: String): Option[Employee] =
    name match {
      case "Joe" => Some(Employee("Joe", "Finances", Some("Julie")))
      case "Mary" => Some(Employee("Mary", "IT", None))
      case "Izumi" => Some(Employee("Izumi", "IT", Some("Mary")))
      case _ => None
    }

  // Exercise 4.2
  def mean (xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  def variance (xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x-m, 2))))
  }







//  def variance(xs: Seq[Double]): Option[Double] = {
//    case => None
//  }


}


case object Chapter4 extends App {
  val joeDepartment: Option[String] =
    Option.lookupByName("Joe").map(_.department)

  val joeManager: Option[String] =
    Option.lookupByName("Joe").map(_.manager.toString)

  val maryDepartment: Option[String] =
    Option.lookupByName("Mary").map(_.department)

  val maryManager: Option[String] =
    Option.lookupByName("Mary").map(_.manager.toString)

  println(joeDepartment)
  println(joeManager)
  println(maryDepartment)
  println(maryManager)

  val joeFlatMap =
    Option.lookupByName("Joe").flatMap(_.manager)
  println(joeFlatMap)


  //Exercise 4.2
  println("\nExercise 4.2")
  val x = Seq(1.1,6,7,1,0)
  val y = Seq()

  println(x)
  println(Option.mean(x))
  println(Option.variance(x))
  println(y)
  println(Option.mean(y))
  println(Option.variance(y))
  //Exercise 4.2 END
  println("Exercise 4.2 End\n")
  val dept: String = Option.lookupByName("Joe").map(_.department).
    filter(_ != "Accounting").getOrElse("Default Dept")
  println(dept)

  //Exercise 4.3
  println("\nExercise 4.3")
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa =>
      b map (bb =>
        f(aa, bb)))


  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

//  def insuranceRateQuote(age: Int,
//                         numberOfSpeedingTickets: Int): Option[Double] ={
//    val optAge: Option[Int] = Try { age.toInt }
//    val optTickets: Option[Int] = Try { numberOfSpeedingTickets.toInt }
//    map2(optAge, optTickets)(insuranceRateQuote)
//  }

  //Exercise 4.4
//  def sequence[A](as: List[Option[A]]): Option[List[A]] = {
//    case Nil => Some(Nil)
//    case h :: t => h.flatMap(hh => sequence(t).map(hh :: _))
//  }

  //Exercise 4.5
//  def traverse[A,B](a: List[A]) (f: A => Option[B]) : Option[List[B]] = {
//
//    def put(elem: A, b: List[Option[B]]) : List[Option[B]] = {
//      b ::: List(f(elem))
//    }
//
//    def traverseOpt(a: List[A], b: List[Option[B]]) : Option[List[B]] = a match{
//      case Nil => sequence(b)
//      case x :: xs => traverseOpt(xs, put(x,b))
//    }
//
//    traverseOpt(a,List[Option[B]]())
//  }
//
//  def sequenceTraverse[A](a: List[Option[A]]) : Option[List[A]] = {
//    traverse(a) (x => x)
//  }


//  def sequence_1[A](as: List[Option[A]]): Option[List[A]] =
//    as.foldRight[Option[List[A]]](Some(Nil))((a, acc) => map2(a, acc)(_ :: _))
//
//  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
//    a match
//  case Nil => Some(Nil)
//  case h::t => map2(f(h), traverse(t)(f))(_ :: _)
//
//  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
//    a.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_ :: _))
//
//
//  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
//    traverse(a)(x => x)

}

