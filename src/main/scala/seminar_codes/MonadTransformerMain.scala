package seminar_codes

import MonadMain.ToMonad

object MonadTransformerMain extends App {

  //Transformer Type
  trait Transformer[M1[_], M2[_]] {
    def bind[A, B](fa: M1[M2[A]])(f: A => M1[M2[B]]): M1[M2[B]]
    def unit[A](a : A) : M1[M2[A]]
    def lift[A](fa : M1[A]): M1[M2[A]]
    def join[A](fa : M1[M2[M1[M2[A]]]]) : M1[M2[A]] = {
      bind(fa)(m => m)
    }
    def map[A, B](fa: M1[M2[A]])(f: A => B): M1[M2[B]] = {
      bind(fa)(a => unit(f(a)))
    }
  }

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

  //Transformer definition
  trait OptionTransformer[M1[_]] extends Transformer[M1, Option]
  implicit object ListOptionTransformer extends OptionTransformer[List] {
    override def bind[A, B](fa: List[Option[A]])(f: A => List[Option[B]]): List[Option[B]]=
      fa.bind {
        case Some(a) => f(a)
        case None => List(None)
      }

    override def unit[A](a: A): List[Option[A]] = List(Some(a))

    override def lift[A](fa: List[A]): List[Option[A]] =
      fa.bind(a => unit(a))
  }

  trait ListTransformer[M1[_]] extends Transformer[M1, List]
  implicit object OptionListTransformer extends ListTransformer[Option] {
    override def bind[A, B](fa: Option[List[A]])(f: A => Option[List[B]]): Option[List[B]] =
      fa.bind {
        case Nil => Some(Nil)
        case x => Some(x.flatMap(f).flatten)
      }

    override def unit[A](a: A): Option[List[A]] = Some(List(a))

    override def lift[A](fa: Option[A]): Option[List[A]] =
      fa.bind(a => unit(a))
  }

  // EXAMPLES

  val l : List[Option[Int]] = List(1, 2, 3).applyLiftT  //List(Some(1), Some(2), Some(3))
  val l0 : List[Option[Int]] = List(Some(1), None, Some(3))
  val l1: List[Option[Int]] = List(None)
  val o : Option[List[Int]] = Some(List(1, 2, 3))

  val temp: List[Option[List[Option[Int]]]] = l.applyMapT(x => List(Some(x + 1)))
  println(l.applyBindT(x => List(Some(x + 1))) == temp.applyJoinT)
  println(l.applyBindT(x => List(Some(x + 1))) == ListOptionTransformer.join(l.applyMapT(x => List(Some(x + 1)))))

  println

  println(l1.applyMapT(_ + 1))

  println

  println(l)
  println(l.applyMapT(_ + 1))
  println(l.applyMapT("str -> " + _ + " <-"))
  println(l.applyBindT(x => List(Some(x - 1), Some(x), Some(x + 1))))
  println(l.applyBindT(x => List(Some("s" + (x-1)), Some("s"+x), Some("s"+(x + 1)))))
  println(l.applyBindT(_ => Nil))
  println(l.applyBindT(_ => List(None)))

  println

  println(l0.applyMapT(_ + 1))
  println(l0.applyBindT(x => List(Some(x - 1), Some(x), Some(x + 1))))

  println

  println(o.applyMapT(_ + 1))
  println(o.applyMapT(_ + 1).applyMapT(_ * 2))
  println(o.applyMapT("str -> " + _ + " <-"))
  println(o.applyBindT(x => Some(List(x - 1, x, x + 1))))
  println(o.applyBindT(x => Some(List(x - 1, x, x + 1))).applyBindT(x => Some(List(x * 2))))
  println(o.applyBindT(x => Some(List("s" + (x-1), "s"+x, "s"+(x + 1)))))
  println(o.applyBindT(_ => None))
}
