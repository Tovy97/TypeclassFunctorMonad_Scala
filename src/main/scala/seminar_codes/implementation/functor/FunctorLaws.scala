package seminar_codes.implementation.functor

import FunctorHelper._

class FunctorLaws[F[_], A](implicit functor: Functor[F]) {
  def identity(fa : F[A]): Boolean = {
    def id (x: A) :A = x
    fa.applyMap(id) == fa
  }
  def composition(fa: F[A])(f: A => A, g: A => A) : Boolean =
    fa.applyMap(f).applyMap(g) == fa.applyMap(g compose f)
}
object FunctorLaws {
  def apply[F[_], A]()(implicit functor: Functor[F]) = new FunctorLaws[F, A]
}
