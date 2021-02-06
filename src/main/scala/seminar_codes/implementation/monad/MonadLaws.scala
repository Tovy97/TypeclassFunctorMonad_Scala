package seminar_codes.implementation.monad

import seminar_codes.implementation.functor.FunctorLaws
import seminar_codes.implementation.monad.MonadHelper._

class MonadLaws[M[_], A](implicit monad: Monad[M]) extends FunctorLaws[M, A]{
  def leftIdentity(a : A)(f : A => M[A]): Boolean = monad.unit(a).applyBind(f) == f(a)
  def rightIdentity(t: M[A]) : Boolean = t.applyBind(t.applyUnit) == t
  def associativity(t: M[A])(f : A => M[A], g : A => M[A]) : Boolean =
    t.applyBind(f).applyBind(g) == t.applyBind((x: A) => f(x).applyBind(g))
  def bindMapJoin(t: M[A])(f : A => M[A]) : Boolean =
    t.applyBind(f) == t.applyMap(f).applyJoin
}
object MonadLaws {
  def apply[M[_], A]()(implicit monad: Monad[M]) = new MonadLaws[M, A]
}