//sealed trait Option[+A]
//case class Some[+A](get: A) extends Option[A]
//case object None extends Option[Nothing]

trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]) : Option[B]
  def getOrElse[B >: A](ob: => Option[B]): Option[B]
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
}

object Chapter4 extends App{

//  def mean(xs: Seq[Double]): Option[Double] =
//    if (xs.isEmpty) None
//    else Some(xs.sum / xs.length)
//
//  val a = Seq(1.0,2,3,4,5)
//  println(mean(a))

  case class Employee(name: String, department: String)
//  def lookupByName(Name:String): Option[Employee] = ...
//  println(lookupByName("Joe").map(_.department))



}
