package wadler_example

import seminar_codes.implementation.monad.MonadHelper._
import seminar_codes.implementation.monad.MonadImplementation.RaiseReturnMonad
import wadler_example.WadlerMain._

object ExceptionExample {

  lazy val withoutMonads: Unit = {
    println("Without monads")

    def excEval(e: Term): RaiseReturn[Int] = e match {
      case Constant(c) => Return(c)
      case Div(t, u) => excEval(t) match {
        case Raise(e) => Raise(e)
        case Return(a) => excEval(u) match {
          case Raise(e) => Raise(e)
          case Return(b) => if (b == 0) Raise("Divide by zero") else Return(a / b)
        }
      }
    }

    try {
      println(excEval(answer))
      println(excEval(error))
    } catch {
      case _: Throwable =>
    }
  }
  lazy val withMonads: Unit = {
    println("With monads")
    implicit val f: (Int, Int, Term) => RaiseReturn[Int] = (a: Int, b: Int, _) => if (b == 0) Raise("Divide by zero") else Return(a / b)
    implicit val g: (Int, Term) => RaiseReturn[Int] = (a: Int, _) => Return(a)
    try {
      println(RaiseReturnMonad.unit(answer).applyBind(eval[RaiseReturn]))
      println(RaiseReturnMonad.unit(error).applyBind(eval[RaiseReturn]))
    } catch {
      case _: Throwable =>
    }
  }
  lazy val example: Unit = {
    println("\nVariation one - Exception")
    withoutMonads
    withMonads
  }

  sealed trait RaiseReturn[A]

  case class Return[A](ris: A) extends RaiseReturn[A]

  case class Raise[A](e: String) extends RaiseReturn[A]

}
