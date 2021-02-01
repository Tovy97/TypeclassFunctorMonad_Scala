package wadler_example

import seminar_codes.MonadMain.{Monad, MonadMethods}
import wadler_example.WadlerMain._

object IdExample {

  lazy val withoutMonads: Unit = {
    println("Without monads")

    def idEval(e: Term): Int = e match {
      case Constant(a) => a
      case Div(t, u) => idEval(t) / idEval(u)
    }

    try {
      println(idEval(answer))
      println(idEval(error))
    } catch {
      case _: Throwable =>
    }
  }
  lazy val withMonads: Unit = {
    println("With monads")
    implicit val f: (Int, Int, Term) => Id[Int] = (a: Int, b: Int, _) => Id(a / b)
    implicit val g: (Int, Term) => Id[Int] = (a: Int, _) => Id(a)
    try {
      println(Id(answer).bind(eval[Id]))
      println(Id(error).bind(eval[Id]))
    } catch {
      case _: Throwable =>
    }
  }
  lazy val example: Unit = {
    println("Variation zero - Id")
    withoutMonads
    withMonads
  }

  case class Id[A](a: A)

  implicit object IdentityMonad extends Monad[Id] {
    override def bind[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = fa match {
      case Id(a) => f(a)
    }

    override def unit[A](a: A): Id[A] = Id(a)
  }
}
