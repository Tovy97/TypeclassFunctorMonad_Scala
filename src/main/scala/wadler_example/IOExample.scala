package wadler_example

import seminar_codes.MonadMain.{Monad, ToMonad}
import wadler_example.WadlerMain._

object IOExample {

  trait IO[O, A] {
    val output : O
    val a : A
  }
  case class IOStr[A](output: String, a: A) extends IO[String, A]

  implicit object IOMonad extends Monad[IOStr] {
    override def bind[A, B](fa: IOStr[A])(f: A => IOStr[B]): IOStr[B] = {
      val IOStr(x, a) = fa
      val IOStr(y, b) = f(a)
      IOStr(x + y, b)
    }

    override def unit[A](a: A): IOStr[A] = IOStr("", a)
  }

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
    implicit val f: (Int, Int, Term) => IOStr[Int] = (a: Int, b: Int, e: Term) => out(line(e, a / b)).bind(_ => IOMonad.unit(a / b))
    implicit val g: (Int, Term) => IOStr[Int] = (a: Int, e: Term) => out(line(e, a)).bind(_ => IOMonad.unit(a))
    try {
      println(IOMonad.unit(answer).bind(eval[IOStr]).output)
      println(IOMonad.unit(error).bind(eval[IOStr]).output)
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
