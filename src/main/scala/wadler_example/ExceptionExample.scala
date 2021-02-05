package wadler_example

import seminar_codes.MonadMain.{Monad, ToMonad1}
import wadler_example.WadlerMain._

object ExceptionExample {

  sealed trait RaiseReturn[A]
  case class Return[A](ris: A) extends RaiseReturn[A]
  case class Raise[A](e: String) extends RaiseReturn[A]

  implicit object RaiseReturnMonad extends Monad[RaiseReturn] {
    override def bind[A, B](fa: RaiseReturn[A])(f: A => RaiseReturn[B]): RaiseReturn[B] = fa match {
      case Raise(e) => Raise(e)
      case Return(a) => f(a)
    }

    override def unit[A](a: A): RaiseReturn[A] = Return(a)
  }

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
}
