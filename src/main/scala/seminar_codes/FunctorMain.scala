package seminar_codes

import seminar_codes.implementation.functor.Functor
import seminar_codes.implementation.functor.FunctorHelper._
import seminar_codes.implementation.functor.FunctorImplementation._

object FunctorMain extends App {

  def applyMap[F[_], A, B](a: F[A])(f: A => B)(implicit functor: Functor[F]): F[B] =
    functor.map(a)(f)

  //EXAMPLES

  println(applyMap(List(1, 2, 3))(_ + 1)) //List(2, 3, 4)
  println(applyMap(Option(1))(_ + 1)) // Some(2)
  //println(applyMap(Set(true, true, false))(!_)) // COMPILE ERROR

  println(List(1, 2, 3).applyMap(_ + 1)) //List(2, 3, 4)
  println(Option(1).applyMap(_ + 1)) // Some(2)
  //println(Set(true, true, false).applyMap(!_)) // COMPILE ERROR

  println()

  //Why are functors useful?
  val a: Option[Int] = Some(20)
  def f(a: Int): Int = if (a % 2 == 0) a / 2 else a - 1

  //without functor
  println(a match {
    case None => None
    case Some(b) => Some(f(b))
  }) //Some(10)

  //with  functor
  println(a.applyMap(f)) //Some(10)
}
