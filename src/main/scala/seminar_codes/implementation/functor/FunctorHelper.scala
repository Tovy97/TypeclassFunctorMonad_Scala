package seminar_codes.implementation.functor

object FunctorHelper {

  implicit class ToFunctor[F[_], A, B](a: F[A])(implicit functor: Functor[F]) {
    def applyMap(f: A => B): F[B] = functor.map(a)(f)
  }

}
