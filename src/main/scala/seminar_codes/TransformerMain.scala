package seminar_codes

import MonadMain.{ListMonad, Monad, ToMonad1}

object TransformerMain extends App {

  //Transformer Type
  trait Transformer[M1[_], M2[_]] {
    implicit def monadM1: Monad[M1]

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

  class TransformerLaws[M1[_], M2[_], A](implicit transformer: Transformer[M1, M2]) {
    def law1(a : A) : Boolean = {
      transformer.monadM1.unit(a).applyLiftT == transformer.unit(a)
    }
    def law2(m: M1[A])(f : A => M1[A]) : Boolean = {
      transformer.lift(transformer.monadM1.bind(m)(f)) == transformer.lift(m).applyBindT(transformer.lift[A] _ compose f)
    }

    def leftIdentity(a : A)(f : A => M1[M2[A]]): Boolean = transformer.unit(a).applyBindT(f) == f(a)
    def rightIdentity(t: M1[M2[A]]) : Boolean = t.applyBindT(t.applyUnitT) == t
    def associativity(t: M1[M2[A]])(f : A => M1[M2[A]], g : A => M1[M2[A]]) : Boolean =
      t.applyBindT(f).applyBindT(g) == t.applyBindT((x: A) => f(x).applyBindT(g))
    def identity(fa : M1[M2[A]]): Boolean = {
      def id (x: A) :A = x
      fa.applyMapT(id) == fa
    }
    def composition(fa: M1[M2[A]])(f: A => A, g: A => A) : Boolean =
      fa.applyMapT(f).applyMapT(g) == fa.applyMapT(g compose f)
    def bindMapJoin(t: M1[M2[A]])(f : A => M1[M2[A]]) : Boolean =
      t.applyBindT(f) == t.applyMapT(f).applyJoinT
  }
  object TransformerLaws {
    def apply[M1[_], M2[_], A]()(implicit transformer: Transformer[M1, M2]) =
      new TransformerLaws[M1,M2,A]
  }

  //Transformer definition
  trait OptionT[M1[_]] extends Transformer[M1, Option] {

    override def bind[A, B](fa: M1[Option[A]])(f: A => M1[Option[B]]): M1[Option[B]]= {
      monadM1.bind(fa) {
        case None => monadM1.unit(None)
        case Some(a) => f(a)
      }
    }

    override def unit[A](a: A): M1[Option[A]] = monadM1.unit(Some(a))

    override def lift[A](fa: M1[A]): M1[Option[A]] =
      monadM1.bind(fa)(a => unit(a))
  }
  implicit object ListOptionT extends OptionT[List] {
    override implicit val monadM1: Monad[List] = implicitly
  }

  // EXAMPLES

  val l : List[Option[Int]] = List(1, 2, 3).applyLiftT  //List(Some(1), Some(2), Some(3))
  val l0 : List[Option[Int]] = List(Some(1), None, Some(3))
  val l1: List[Option[Int]] = List(None)

  println()

  println(l1.applyMapT(_ + 1))

  println()

  println(l)
  println(l.applyMapT(_ + 1))
  println(l.applyMapT("str -> " + _ + " <-"))
  println(l.applyBindT(x => List(Some(x - 1), Some(x), Some(x + 1))))
  println(l.applyBindT(x => List(Some("s" + (x-1)), Some("s"+x), Some("s"+(x + 1)))))
  println(l.applyBindT(_ => Nil))
  println(l.applyBindT(_ => List(None)))

  println()

  println(l0.applyMapT(_ + 1))
  println(l0.applyBindT(x => List(Some(x - 1), Some(x), Some(x + 1))))

  println()

  //Monad VS Transformer

  //Functors compose
  val v = List(Option(1),None,Option(3))
  val f1 : Int => String = _ + "a"
  println(v.applyMap(x => x.applyMap(f1)))

  //Monads don't compose
  val f2 : Int => List[Option[String]] = x => List(Some(x + "a"))
  //println(v.applyBind(x => x.applyBind(f2)))
  /*
  type mismatch;
   found   : Int => List[Option[String]]
   required: Int => Option[?]
  */
  def helper[A,B](d : B)(f : A => B) : Option[A] => B = {
    case None => d
    case Some(y) => f(y)
  }
  val default : List[Option[String]] = ListMonad.unit(None)
  println(v.applyBind(helper(default)(f2)))
  println(v.applyBind {
    case None => default
    case Some(x) => f2(x)
  })
  //Transformer
  println(v.applyBindT(f2))
}
