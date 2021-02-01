package wadler_example

import seminar_codes.MonadMain.{Monad, MonadMethods}
import wadler_example.WadlerMain._

object StateExample {

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

  implicit object StateMonad extends Monad[State] {
    override def bind[A, B](fa: State[A])(f: A => State[B]): State[B] = {
      val function = (x: Int) => {
        val (a, y) = fa(x)
        val (b, z) = f(a)(y)
        (b, z)
      }
      State(function)
    }

    override def unit[A](a: A): State[A] = State((a, _))
  }
  lazy val withMonads: Unit = {
    println("With monads")
    val tick = State((x: Int) => ((), x + 1))
    implicit val f: (Int, Int, Term) => State[Int] = (a: Int, b: Int, _) => tick.bind(Unit => StateMonad.unit(a / b))
    implicit val g: (Int, Term) => State[Int] = (a: Int, _) => StateMonad.unit(a)
    try {
      println(StateMonad.unit(answer).bind(eval[State])(0))
      println(StateMonad.unit(error).bind(eval[State])(0))
    } catch {
      case _: Throwable =>
    }
  }
  lazy val example: Unit = {
    println("\nVariation two - State")
    withoutMonads
    withMonads
  }

  case class State[A](func: Int => (A, Int)) {
    def apply(x: Int): (A, Int) = func(x)
  }
}
