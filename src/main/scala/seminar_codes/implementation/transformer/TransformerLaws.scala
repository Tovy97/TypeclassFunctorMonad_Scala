package seminar_codes.implementation.transformer

import seminar_codes.implementation.transformer.TransformerHelper._

class TransformerLaws[M1[_], M2[_], A](implicit transformer: Transformer[M1, M2]) {
  def law1(a: A): Boolean = {
    transformer.monadM1.unit(a).applyLiftT == transformer.unit(a)
  }

  def law2(m: M1[A])(f: A => M1[A]): Boolean = {
    transformer.lift(transformer.monadM1.bind(m)(f)) == transformer.lift(m).applyBindT(transformer.lift[A] _ compose f)
  }

  def leftIdentity(a: A)(f: A => M1[M2[A]]): Boolean = transformer.unit(a).applyBindT(f) == f(a)

  def rightIdentity(t: M1[M2[A]]): Boolean = t.applyBindT(t.applyUnitT) == t

  def associativity(t: M1[M2[A]])(f: A => M1[M2[A]], g: A => M1[M2[A]]): Boolean =
    t.applyBindT(f).applyBindT(g) == t.applyBindT((x: A) => f(x).applyBindT(g))

  def identity(fa: M1[M2[A]]): Boolean = {
    def id(x: A): A = x

    fa.applyMapT(id) == fa
  }

  def composition(fa: M1[M2[A]])(f: A => A, g: A => A): Boolean =
    fa.applyMapT(f).applyMapT(g) == fa.applyMapT(g compose f)

  def bindMapJoin(t: M1[M2[A]])(f: A => M1[M2[A]]): Boolean =
    t.applyBindT(f) == t.applyMapT(f).applyJoinT
}

object TransformerLaws {
  def apply[M1[_], M2[_], A]()(implicit transformer: Transformer[M1, M2]) =
    new TransformerLaws[M1, M2, A]
}
