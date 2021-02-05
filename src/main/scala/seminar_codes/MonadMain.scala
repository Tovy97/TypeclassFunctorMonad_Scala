package seminar_codes

import FunctorMain.{Functor, FunctorLaws, toFunctor2}

object MonadMain extends App {

  //Monad Type
  trait Monad[M[_]] extends Functor[M]{
    def bind[A, B](fa: M[A])(f: A => M[B]): M[B] //also called flatMap

    def unit[A](a: A): M[A] //also called apply

    override def map[A, B](fa: M[A])(f: A => B): M[B] = {
      bind(fa)(a => unit(f(a)))
    }

    override def join[A](ma: M[M[A]]): M[A] = { //also called flatten
      bind(ma)(m => m)
    }
  }

  implicit class ToMonad[M[_], A, B](a: M[A])(implicit monad: Monad[M]) {
    def applyBind(f: A => M[B]): M[B] = monad.bind(a)(f)

    def applyMap(f: A => B): M[B] = monad.map(a)(f)

    lazy val applyUnit: A => M[A] = monad.unit
  }

  def bind[M[_], A, B](a: M[A])(f: A => M[B])(implicit monad: Monad[M]): M[B] = monad.bind(a)(f)

  class MonadLaws[M[_], A](implicit monad: Monad[M]) extends FunctorLaws[M, A]{
    def leftIdentity(a : A)(f : A => M[A]): Boolean = monad.unit(a).applyBind(f) == f(a)
    def rightIdentity(t: M[A]) : Boolean = t.applyBind(t.applyUnit) == t
    def associativity(t: M[A])(f : A => M[A], g : A => M[A]) : Boolean =
      t.applyBind(f).applyBind(g) == t.applyBind((x: A) => f(x).applyBind(g))
    def bindMapJoin(t: M[A])(f : A => M[A]) : Boolean =
      t.applyBind(f) == t.applyMap(f).applyJoin
  }
  object MonadLaws {
    def apply[M[_], A]()(implicit monad: Monad[M]) = new MonadLaws[M, A]
  }

  //Monads definition
  implicit object ListMonad extends Monad[List] {
    override def bind[A, B](fa: List[A])(f: A => List[B]): List[B] =
      fa.flatMap(f)

    override def unit[A](a: A): List[A] = List(a)
  }

  implicit object OptionMonad extends Monad[Option] {
    override def bind[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa match {
        case None => None
        case Some(a) => f(a)
      }

    override def unit[A](a: A): Option[A] = Some(a)
  }

  //EXAMPLES

  val a: Option[Int] = Some(20)

  println(bind(List(1, 2, 3))((x: Int) => List(x + 1))) //List(2, 3, 4)
  println(bind(Some(1): Option[Int])((x: Int) => Some(x + 1))) // Some(2)
  //println(bind(Set(true, true, false))((x : Boolean) => Set(!x))) // COMPILE ERROR

  println(List(1, 2, 3).applyBind((x: Int) => List(x + 1))) //List(2, 3, 4)
  println((Some(1): Option[Int]).applyBind((x: Int) => Some(x + 1))) // Some(2)
  //println(Set(true, true, false).applyBind((x : Boolean) => Set(!x))) // COMPILE ERROR

  println("---")

  //FUNCTORS VS MONADS

  def f(x: Int): Option[Int] = if (x == 0) None else Some(2 / x)

  val temp = a.applyMap(_ + 1) //Some(21)
  println(temp)
  println(temp.applyMap(_ + 1)) //Some(22)

  println("---")

  val temp1 = a.applyMap(f) //Some(Some(0))
  val temp2 = a.applyBind(f)   //Some(0)

  println(temp1)
  println(temp2)

  //println(temp1.applyMap(f)) //COMPILE ERROR

  println(temp2.applyBind(f)) // None

}
