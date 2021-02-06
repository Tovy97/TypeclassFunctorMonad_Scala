package seminar_codes.implementation.functor

import tree_example.implementation.{Tree, Empty, Node}

object FunctorImplementation {
  implicit object ListFunctor extends Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  implicit object OptionFunctor extends Functor[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] =
      fa match {
        case None => None
        case Some(a) => Some(f(a))
      }
  }

  implicit object TreeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Empty => Empty
      case Node(left, value, right) =>
        Node(map(left)(f), f(value), map(right)(f))
    }
  }
}
