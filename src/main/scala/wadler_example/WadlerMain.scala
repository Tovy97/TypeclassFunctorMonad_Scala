package wadler_example

import seminar_codes.implementation.monad.Monad
import seminar_codes.implementation.monad.MonadHelper._

object WadlerMain extends App {

  lazy val answer = Div(Div(Constant(1972), Constant(2)), Constant(23))
  lazy val error = Div(Constant(1), Constant(0))

  def eval[M[_]](e: Term)(implicit f: (Int, Int, Term) => M[Int], g: (Int, Term) => M[Int], monad: Monad[M]): M[Int] = e match {
    case Constant(a) => g(a, e)
    case Div(t, u) => eval(t).applyBind((a: Int) => eval(u).applyBind((b: Int) => f(a, b, e)))
  }

  sealed trait Term

  case class Constant(c: Int) extends Term

  case class Div(t1: Term, t2: Term) extends Term

  IdExample.example
  ExceptionExample.example
  StateExample.example
  IOExample.example
}

