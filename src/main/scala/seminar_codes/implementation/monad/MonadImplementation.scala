package seminar_codes.implementation.monad

import wadler_example.ExceptionExample.{Raise, RaiseReturn, Return}
import wadler_example.IOExample.IOStr
import wadler_example.IdExample.Id
import wadler_example.StateExample.IntState

object MonadImplementation {

  implicit object ListMonad extends Monad[List] {
    override def bind[A, B](fa: List[A])(f: A => List[B]): List[B] =
      fa.flatMap(f)

    override def unit[A](a: A): List[A] = List(a)
  }

  implicit object OptionMonad extends Monad[Option] {
    override def bind[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa match {
        case None => None
        case Some(a) => f(a)
      }

    override def unit[A](a: A): Option[A] = Some(a)
  }

  implicit object RaiseReturnMonad extends Monad[RaiseReturn] {
    override def bind[A, B](fa: RaiseReturn[A])(f: A => RaiseReturn[B]): RaiseReturn[B] = fa match {
      case Raise(e) => Raise(e)
      case Return(a) => f(a)
    }

    override def unit[A](a: A): RaiseReturn[A] = Return(a)
  }

  implicit object IdentityMonad extends Monad[Id] {
    override def bind[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = fa match {
      case Id(a) => f(a)
    }

    override def unit[A](a: A): Id[A] = Id(a)
  }

  implicit object IOStrMonad extends Monad[IOStr] {
    override def bind[A, B](fa: IOStr[A])(f: A => IOStr[B]): IOStr[B] = {
      val IOStr(x, a) = fa
      val IOStr(y, b) = f(a)
      IOStr(x + y, b)
    }

    override def unit[A](a: A): IOStr[A] = IOStr("", a)
  }

  implicit object IntStateMonad extends Monad[IntState] {
    override def bind[A, B](fa: IntState[A])(f: A => IntState[B]): IntState[B] = {
      val function: Int => (B, Int) = (x: Int) => {
        val (a, y) = fa(x)
        val (b, z) = f(a)(y)
        (b, z)
      }
      IntState(function)
    }

    override def unit[A](a: A): IntState[A] = IntState((a, _))
  }

}
