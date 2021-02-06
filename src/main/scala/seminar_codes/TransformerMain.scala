package seminar_codes

import seminar_codes.implementation.transformer.TransformerHelper._
import seminar_codes.implementation.monad.MonadImplementation._
import seminar_codes.implementation.monad.MonadHelper._
import seminar_codes.implementation.transformer.TransformerImplementation._

object TransformerMain extends App {
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