package wadler_example

import seminar_codes.implementation.monad.MonadHelper._
import seminar_codes.implementation.monad.MonadImplementation.IdentityMonad
import wadler_example.WadlerMain._

object IdExample {

  case class Id[A](a: A)

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
      println(IdentityMonad.unit(answer).applyBind(eval[Id]))
      println(IdentityMonad.unit(error).applyBind(eval[Id]))
    } catch {
      case _: Throwable =>
    }
  }
  lazy val example: Unit = {
    println("Variation zero - Id")
    withoutMonads
    withMonads
  }
}
