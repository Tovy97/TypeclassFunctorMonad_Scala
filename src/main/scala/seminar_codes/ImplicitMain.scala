package seminar_codes
import scala.math.pow

object ImplicitMain extends App {
  //Extension method
  implicit class PowerInt(i : Int) {
    def ** (exp:Int) : Int = pow(i, exp).toInt
    def inc : Int = i + 1
  }
  println(2**7) //128
  println(2.inc) //3

  //Implicit parameter
  implicit val square: Int => Int = (x:Int) => x * x
  def applyFunction(value : Int)(implicit f : Int => Int) : Int = {
    f(value)
  }
  println(applyFunction(3))//9

  //Implicit object
  trait Test {
    def square(x : Int) : Int
  }
  implicit object TestObj extends Test {
    override def square(x : Int) : Int = x * x
  }
  def applyFunction2(value : Int)(implicit t : Test) : Int = {
    t.square(value)
  }
  println(applyFunction2(5)) //25
}
