package tree_example.implementation

sealed trait Tree[+A] {
  override lazy val toString: String = {
    this match {
      case Empty => "."
      case Node(sx, el, dx) => "(" + sx.toString + el + dx.toString + ")"
    }
  }
}
case object Empty extends Tree[Nothing]
case class Node[A](left : Tree[A], value : A, right : Tree[A]) extends Tree[A]
