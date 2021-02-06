package seminar_codes

import implementation.monad.Monad
import implementation.monad.MonadImplementation._
import implementation.monad.MonadHelper._

object MonadMain extends App {

  def bind[M[_], A, B](a: M[A])(f: A => M[B])(implicit monad: Monad[M]): M[B] = monad.bind(a)(f)

  //EXAMPLES

  val a: Option[Int] = Some(20)

  println(bind(List(1, 2, 3))((x: Int) => List(x + 1))) //List(2, 3, 4)
  println(bind(Some(1): Option[Int])((x: Int) => Some(x + 1))) // Some(2)
  //println(bind(Set(true, true, false))((x : Boolean) => Set(!x))) // COMPILE ERROR

  println(List(1, 2, 3).applyBind((x: Int) => List(x + 1))) //List(2, 3, 4)
  println((Some(1): Option[Int]).applyBind((x: Int) => Some(x + 1))) // Some(2)
  //println(Set(true, true, false).applyBind((x : Boolean) => Set(!x))) // COMPILE ERROR

  println("---")

  //FUNCTORS VS MONADS

  def f(x: Int): Option[Int] = if (x == 0) None else Some(2 / x)

  val temp = a.applyMap(_ + 1) //Some(21)
  println(temp)
  println(temp.applyMap(_ + 1)) //Some(22)

  println("---")

  val temp1 = a.applyMap(f) //Some(Some(0))
  val temp2 = a.applyBind(f)   //Some(0)

  println(temp1)
  println(temp2)

  //println(temp1.applyMap(f)) //COMPILE ERROR

  println(temp2.applyBind(f)) // None

}
