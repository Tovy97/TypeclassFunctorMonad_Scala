package seminar_codes.implementation.monad

import seminar_codes.implementation.functor.Functor

trait Monad[M[_]] extends Functor[M] {
  def bind[A, B](fa: M[A])(f: A => M[B]): M[B] //also called flatMap

  def unit[A](a: A): M[A] //also called apply

  final def join[A](ma: M[M[A]]): M[A] = { //also called flatten
    bind(ma)(m => m)
  }

  override final def map[A, B](fa: M[A])(f: A => B): M[B] = {
    bind(fa)(a => unit(f(a)))
  }
}
