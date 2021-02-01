package seminar_codes

object FunctorMain extends App {

  val a: Option[Int] = Some(20)

  implicit object ListFunctor extends Functor[List] {
    override def fmap[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  implicit object OptionFunctor extends Functor[Option] {
    override def fmap[A, B](fa: Option[A])(f: A => B): Option[B] =
      fa match {
        case None => None
        case Some(a) => Some(f(a))
      }
  }

  def applyMap[F[_], A, B](a: F[A])(f: A => B)(implicit functor: Functor[F]): F[B] = {
    functor.fmap(a)(f)
  }

  implicit class FunctorMethod[F[_], A, B](a: F[A])(implicit functor: Functor[F]) {
    def applyMap(f: A => B): F[B] = functor.fmap(a)(f)
  }

  println(applyMap(List(1, 2, 3))(_ + 1)) //List(2, 3, 4)
  println(applyMap(Some(1): Option[Int])(_ + 1)) // Some(2)
  //println(applyMap(Set(true, true, false))(!_)) // COMPILE ERROR

  println(List(1, 2, 3).applyMap(_ + 1)) //List(2, 3, 4)
  println((Some(1): Option[Int]).applyMap(_ + 1)) // Some(2)
  //println(Set(true, true, false).applyMap(!_)) // COMPILE ERROR

  println("---")

  def f(a: Int): Int = if (a % 2 == 0) a / 2 else a - 1

  trait Functor[F[_]] {
    def fmap[A, B](fa: F[A])(f: A => B): F[B]
  }

  println(a match {
    case None => None
    case Some(b) => Some(f(b))
  })
  println(a.applyMap(f))
}
