package wadler_example

import seminar_codes.MonadMain.{Monad, ToMonad}
import wadler_example.WadlerMain._

object IOExample_TC {

  implicit object IOMonad extends Monad[IO] {
    override def bind[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = {
      val IO(x, a) = fa
      val IO(y, b) = f(a)
      IO(x + y, b)
    }

    override def unit[A](a: A): IO[A] = IO("", a)

    override def map[A, B](fa: IO[A])(f: A => B): IO[B] = {
      val IO(x, a) = fa
      val b = f(a)
      IO(x, b)
    }
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
    val out = (x: String) => IO(x, ())
    implicit val f: (Int, Int, Term) => IO[Int] = (a: Int, b: Int, e: Term) => out(line(e, a / b)).bind(_ => IOMonad.unit(a / b))
    implicit val g: (Int, Term) => IO[Int] = (a: Int, e: Term) => out(line(e, a)).bind(_ => IOMonad.unit(a))
    try {
      println(IOMonad.unit(answer).bind(eval[IO]).Output)
      println(IOMonad.unit(error).bind(eval[IO]).Output)
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

  case class IO[A](Output: String, a: A)

}
