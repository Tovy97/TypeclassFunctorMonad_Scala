package seminar_codes.implementation.transformer

import seminar_codes.implementation.monad.Monad
import seminar_codes.implementation.monad.MonadImplementation.ListMonad

object TransformerImplementation {

  trait OptionT[M1[_]] extends Transformer[M1, Option] {

    override def bind[A, B](fa: M1[Option[A]])(f: A => M1[Option[B]]): M1[Option[B]] = {
      monadM1.bind(fa) {
        case None => monadM1.unit(None)
        case Some(a) => f(a)
      }
    }

    override def lift[A](fa: M1[A]): M1[Option[A]] =
      monadM1.bind(fa)(a => unit(a))

    override def unit[A](a: A): M1[Option[A]] = monadM1.unit(Some(a))
  }

  implicit object ListOptionT extends OptionT[List] {
    override implicit val monadM1: Monad[List] = implicitly
  }

}
