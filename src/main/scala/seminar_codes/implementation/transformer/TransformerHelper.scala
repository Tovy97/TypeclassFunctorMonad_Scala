package seminar_codes.implementation.transformer

object TransformerHelper {
  implicit class ToTransformer1[M1[_], M2[_], A, B](a: M1[M2[A]])(implicit transformer: Transformer[M1, M2]) {
    lazy val applyUnitT: A => M1[M2[A]] = transformer.unit
    def applyMapT(f: A => B): M1[M2[B]] = transformer.map(a)(f)
    def applyBindT(f: A => M1[M2[B]]): M1[M2[B]] = transformer.bind(a)(f)
  }
  implicit class ToTransformer2[M1[_], M2[_], A, B](fa : M1[A])(implicit transformer: Transformer[M1, M2]) {
    lazy val applyLiftT: M1[M2[A]] = transformer.lift(fa)
  }
  implicit class ToTransformer3[M1[_], M2[_], A, B](fa : M1[M2[M1[M2[A]]]])(implicit transformer: Transformer[M1, M2]) {
    lazy val applyJoinT: M1[M2[A]] = transformer.join(fa)
  }
}