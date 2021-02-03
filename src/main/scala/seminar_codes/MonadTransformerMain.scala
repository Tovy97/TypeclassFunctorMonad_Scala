package seminar_codes

import FunctorMain.Functor
import FunctorMain.FunctorMethod
import MonadMain.Monad
import MonadMain.MonadMethods

object MonadTransformerMain extends App {

  trait Transformer[A[_], B[_]] {
    def mapT[T, C](fa: A[B[T]])(f: T => C)(implicit func1 : Functor[List], func2 : Functor[Option]): A[B[C]]

    def bindT[T, C](fa: A[B[T]])(f: T => A[B[C]])(implicit mon : Monad[List]): A[B[C]]
  }

  trait OptionTransformer[A[_]] extends Transformer[A, Option]
  trait ListTransformer[A[_]] extends Transformer[A, List]

  implicit object OptionListTransformer extends OptionTransformer[List] {
    override def mapT[T, C](fa: List[Option[T]])(f: T => C)(implicit func1 : Functor[List], func2 : Functor[Option]): List[Option[C]] =
      fa.applyMap(opt => opt.applyMap(f))

    override def bindT[T, C](fa: List[Option[T]])(f: T => List[Option[C]])(implicit mon : Monad[List]): List[Option[C]] =
      fa.bind {
        case Some(a) => f(a)
        case None => List(None)
      }
  }

  implicit object ListOptionTransformer extends ListTransformer[Option] {
    override def mapT[T, C](fa: Option[List[T]])(f: T => C)(implicit func1: Functor[List], func2: Functor[Option]): Option[List[C]] =
      fa.applyMap(list => list.applyMap(f))

    override def bindT[T, C](fa: Option[List[T]])(f: T => Option[List[C]])(implicit mon: Monad[List]): Option[List[C]] =
      fa.bind {
        case Nil => Some(Nil)
        case x => Some(x.flatMap(f).flatten)
      }
  }

  implicit class TransformerMethods[A[_], B[_], T](a: A[B[T]])(implicit transformer: Transformer[A, B]) {
    def applyMapT[C](f: T => C): A[B[C]] = transformer.mapT(a)(f)

    def applyBindT[C](f: T => A[B[C]]): A[B[C]] = transformer.bindT(a)(f)
  }

  val l : List[Option[Int]] = List(Some(1), Some(2), Some(3))
  val o : Option[List[Int]] = Some(List(1, 2, 3))

  println(l.applyMapT(_ + 1))
  println(l.applyMapT("str -> " + _ + " <-"))
  println(l.applyBindT(x => List(Some(x - 1), Some(x), Some(x + 1))))
  println(l.applyBindT(x => List(Some("s" + (x-1)), Some("s"+x), Some("s"+(x + 1)))))
  println(l.applyBindT(_ => Nil))
  println(l.applyBindT(_ => List(None)))

  println

  println(o.applyMapT(_ + 1))
  println(o.applyMapT("str -> " + _ + " <-"))
  println(o.applyBindT(x => Some(List(x - 1, x, x + 1))))
  println(o.applyBindT(x => Some(List("s" + (x-1), "s"+x, "s"+(x + 1)))))
  println(o.applyBindT(_ => None))
}
