package wadler_example

import seminar_codes.implementation.monad.MonadHelper._
import seminar_codes.implementation.monad.MonadImplementation.IOStrMonad
import wadler_example.WadlerMain._

object IOExample {

  trait IO[O, A] {
    val output : O
    val a : A
  }
  case class IOStr[A](output: String, a: A) extends IO[String, A]



  lazy val withoutMonads: Unit = {
    println("Without monads")

    def ioEval(e: Term): (String, Int) = e match {
      case Constant(a) => (line(Constant(a), a), a)
      case Div(t, u) =>
        val (x, a) = ioEval(t)
        val (y, b) = ioEval(u)
        (x + y + line(Div(t, u), a / b), a / b)
    }

    try {
      println(ioEval(answer)._1)
      println(ioEval(error)._1)
    } catch {
      case _: Throwable =>
    }
  }
  lazy val withMonads: Unit = {
    println("With monads")
    val out = (x: String) => IOStr(x, ())
    implicit val f: (Int, Int, Term) => IOStr[Int] = (a: Int, b: Int, e: Term) => out(line(e, a / b)).applyBind(_ => IOStrMonad.unit(a / b))
    implicit val g: (Int, Term) => IOStr[Int] = (a: Int, e: Term) => out(line(e, a)).applyBind(_ => IOStrMonad.unit(a))
    try {
      println(IOStrMonad.unit(answer).applyBind(eval[IOStr]).output)
      println(IOStrMonad.unit(error).applyBind(eval[IOStr]).output)
    } catch {
      case _: Throwable =>
    }
  }
  lazy val example: Unit = {
    println("\nVariation three - IO")
    withoutMonads
    withMonads
  }

  def line(t: Term, a: Int): String = "eval(" + t + ") <= " + a + "\n"
}
