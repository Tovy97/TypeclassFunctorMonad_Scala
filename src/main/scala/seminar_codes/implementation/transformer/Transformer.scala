package seminar_codes.implementation.transformer

import seminar_codes.implementation.monad.Monad

trait Transformer[M1[_], M2[_]] {
  implicit def monadM1: Monad[M1]

  def bind[A, B](fa: M1[M2[A]])(f: A => M1[M2[B]]): M1[M2[B]]

  def unit[A](a: A): M1[M2[A]]

  def lift[A](fa: M1[A]): M1[M2[A]]

  def join[A](fa: M1[M2[M1[M2[A]]]]): M1[M2[A]] = {
    bind(fa)(m => m)
  }

  def map[A, B](fa: M1[M2[A]])(f: A => B): M1[M2[B]] = {
    bind(fa)(a => unit(f(a)))
  }
}
