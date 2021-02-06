package seminar_codes

import seminar_codes.implementation.functor.Functor
import seminar_codes.implementation.functor.FunctorImplementation._
import seminar_codes.implementation.functor.FunctorHelper._

object FunctorMain extends App {

  def applyMap[F[_], A, B](a: F[A])(f: A => B)(implicit functor: Functor[F]): F[B] =
    functor.map(a)(f)

  //------EXAMPLES------

  println(applyMap(List(1, 2, 3))(_ + 1)) //List(2, 3, 4)
  println(applyMap(Some(1): Option[Int])(_ + 1)) // Some(2)
  //println(applyMap(Set(true, true, false))(!_)) // COMPILE ERROR

  println(List(1, 2, 3).applyMap(_ + 1)) //List(2, 3, 4)
  println((Some(1): Option[Int]).applyMap(_ + 1)) // Some(2)
  //println(Set(true, true, false).applyMap(!_)) // COMPILE ERROR

  println("---")

  val a: Option[Int] = Some(20)
  def f(a: Int): Int = if (a % 2 == 0) a / 2 else a - 1

  println(a match {
    case None => None
    case Some(b) => Some(f(b))
  })
  println(a.applyMap(f))
}
