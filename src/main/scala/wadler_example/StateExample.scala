package wadler_example

import seminar_codes.MonadMain.{Monad, ToMonad1}
import wadler_example.WadlerMain._

object StateExample {

  trait State[A, S] {
    val func: S => (A, S)
  }
  case class IntState[A](func: Int => (A, Int)) extends State[A, Int] {
    def apply(x: Int): (A, Int) = func(x)
  }

  implicit object IntStateMonad extends Monad[IntState] {
    override def bind[A, B](fa: IntState[A])(f: A => IntState[B]): IntState[B] = {
      val function : Int => (B, Int) = (x: Int) => {
        val (a, y) = fa(x)
        val (b, z) = f(a)(y)
        (b, z)
      }
      IntState(function)
    }

    override def unit[A](a: A): IntState[A] = IntState((a, _))
  }

  lazy val withoutMonads: Unit = {
    println("Without monads")

    def stateEval(e: Term)(x: Int): (Int, Int) = e match {
      case Constant(a) => (a, x)
      case Div(t, u) =>
        val (a, y) = stateEval(t)(x)
        val (b, z) = stateEval(u)(y)
        (a / b, z + 1)
    }

    try {
      println(stateEval(answer)(0))
      println(stateEval(error)(0))
    } catch {
      case _: Throwable =>
    }
  }
  lazy val withMonads: Unit = {
    println("With monads")
    val tick = IntState((x: Int) => ((), x + 1))
    implicit val f: (Int, Int, Term) => IntState[Int] = (a: Int, b: Int, _) => tick.applyBind(_ => IntStateMonad.unit(a / b))
    implicit val g: (Int, Term) => IntState[Int] = (a: Int, _) => IntStateMonad.unit(a)
    try {
      println(IntStateMonad.unit(answer).applyBind(eval[IntState])(0))
      println(IntStateMonad.unit(error).applyBind(eval[IntState])(0))
    } catch {
      case _: Throwable =>
    }
  }
  lazy val example: Unit = {
    println("\nVariation two - State")
    withoutMonads
    withMonads
  }
}