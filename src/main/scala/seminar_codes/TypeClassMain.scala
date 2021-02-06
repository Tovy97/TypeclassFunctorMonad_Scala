package seminar_codes

object TypeClassMain extends App {

  def sumAll[T](l: List[T])(implicit summable: Summable[T]): T =
    summable.sumElements(l)

  implicit class ListSummable[A](l: List[A])(implicit summable: Summable[A]) {
    def sumAll: A = summable.sumElements(l)
  }

  sealed trait Summable[T] {
    def sumElements(list: List[T]): T
  }

  implicit object IntSummable extends Summable[Int] {
    override def sumElements(list: List[Int]): Int = list.sum
  }

  implicit object StringSummable extends Summable[String] {
    override def sumElements(list: List[String]): String = list.mkString("")
  }

  println(sumAll(List(1, 2, 3))) //6
  println(sumAll(List("Scala ", "is ", "awesome"))) // "Scala is awesome"
  //println(sumAll(List(true, true, false))) // COMPILE ERROR

  println(List(1, 2, 3).sumAll) //6
  println(List("Scala ", "is ", "awesome").sumAll) // "Scala is awesome"
  //println(List(true, true, false).sumAll) // COMPILE ERROR
}
