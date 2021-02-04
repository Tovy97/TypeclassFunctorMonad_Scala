package seminar_codes

object FunctorMain extends App {

  //Functors Type
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
    def join[A](fa : F[F[A]]) : F[A] //also called flatten
  }

  implicit class toFunctor1[F[_], A, B](a: F[A])(implicit functor: Functor[F]) {
    def applyMap(f: A => B): F[B] = functor.map(a)(f)
  }
  implicit class toFunctor2[F[_], A](a: F[F[A]])(implicit functor: Functor[F]) {
    def applyJoin: F[A] = functor.join(a)
  }

  def applyMap[F[_], A, B](a: F[A])(f: A => B)(implicit functor: Functor[F]): F[B] =
    functor.map(a)(f)
  def applyJoin[F[_], A](a: F[F[A]])(implicit functor: Functor[F]): F[A] =
    functor.join(a)

  //Functors Definition

  implicit object ListFunctor extends Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)

    override def join[A](fa: List[List[A]]): List[A] = fa.flatten
  }

  implicit object OptionFunctor extends Functor[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] =
      fa match {
        case None => None
        case Some(a) => Some(f(a))
      }

    override def join[A](fa: Option[Option[A]]): Option[A] = fa match {
      case None => None
      case Some(x) => x
    }
  }

  //------EXAMPLES------

  println(applyMap(List(1, 2, 3))(_ + 1)) //List(2, 3, 4)
  println(applyMap(Some(1): Option[Int])(_ + 1)) // Some(2)
  //println(applyMap(Set(true, true, false))(!_)) // COMPILE ERROR

  println(List(1, 2, 3).applyMap(_ + 1)) //List(2, 3, 4)
  println((Some(1): Option[Int]).applyMap(_ + 1)) // Some(2)
  //println(Set(true, true, false).applyMap(!_)) // COMPILE ERROR

  println("---")

  val a: Option[Int] = Some(20)
  def f(a: Int): Int = if (a % 2 == 0) a / 2 else a - 1

  println(a match {
    case None => None
    case Some(b) => Some(f(b))
  })
  println(a.applyMap(f))
}
