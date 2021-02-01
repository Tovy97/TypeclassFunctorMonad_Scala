package seminar_codes

import seminar_codes.FunctorMain._

object MonadMain extends App {

  val a: Option[Int] = Some(20)

  implicit object ListMonad extends Monad[List] {
    override def bind[A, B](fa: List[A])(f: A => List[B]): List[B] =
      fa.flatMap(f)

    override def unit[A](a: A): List[A] = List(a)
  }

  implicit object OptionMonad extends Monad[Option] {
    override def bind[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa match {
        case None => None
        case Some(a) => f(a)
      }

    override def unit[A](a: A): Option[A] = Some(a)
  }

  val temp = a.applyMap(_ + 1)

  implicit class MonadMethods[M[_], A, B](a: M[A])(implicit monad: Monad[M]) {
    def bind(f: A => M[B]): M[B] = monad.bind(a)(f)

    def unit: A => M[A] = monad.unit
  }

  println(bind(List(1, 2, 3))((x: Int) => List(x + 1))) //List(2, 3, 4)
  println(bind(Some(1): Option[Int])((x: Int) => Some(x + 1))) // Some(2)
  //println(bind(Set(true, true, false))((x : Boolean) => Set(!x))) // COMPILE ERROR

  println(List(1, 2, 3).bind((x: Int) => List(x + 1))) //List(2, 3, 4)
  println((Some(1): Option[Int]).bind((x: Int) => Some(x + 1))) // Some(2)
  //println(Set(true, true, false).applyBind((x : Boolean) => Set(!x))) // COMPILE ERROR

  println("---")
  val temp1 = a.applyMap(f)
  //println(temp1.applyMap(f))
  val temp2 = a.bind(f)
  println(temp)
  println(temp.applyMap(_ + 1))

  println("---")

  def bind[M[_], A, B](a: M[A])(f: A => M[B])(implicit monad: Monad[M]): M[B] = {
    monad.bind(a)(f)
  }

  def f(x: Int): Option[Int] = if (x == 0) None else Some(2 / x)

  println(temp1)

  trait Monad[M[_]] {
    def bind[A, B](fa: M[A])(f: A => M[B]): M[B]

    def unit[A](a: A): M[A]
  }

  println(temp2)
  println(temp2.bind(f))

}
