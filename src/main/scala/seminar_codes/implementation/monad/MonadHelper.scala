package seminar_codes.implementation.monad

object MonadHelper {

  implicit class ToMonad1[M[_], A, B](a: M[A])(implicit monad: Monad[M]) {
    lazy val applyUnit: A => M[A] = monad.unit

    def applyBind(f: A => M[B]): M[B] = monad.bind(a)(f)

    def applyMap(f: A => B): M[B] = monad.map(a)(f)
  }

  implicit class ToMonad2[M[_], A](a: M[M[A]])(implicit monad: Monad[M]) {
    lazy val applyJoin: M[A] = monad.join(a)
  }

}
