package tree_example

import seminar_codes.FunctorMain.Functor

import scala.annotation.tailrec

object TreeMain extends App {
  import Converter.ToBST

  implicit object IntBST extends BST[Int]
  implicit object IntTree extends OperationOnTree[Int]

  val tree : Tree[Int] = IntBST.getEmpty.insertAll(List(1,4,7,3,6,4,9,3,8,3,8,0,9,2))
  println(tree)
  println(tree.isMember(4))
  println(tree.toList)
  println(tree.delete(9))

  implicit object TreeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Empty => Empty
      case Node(left, value, right) =>
        Node(map(left)(f), f(value), map(right)(f))
    }
  }
}

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

object Converter {
  implicit class ToTree[A](t : Tree[A])(implicit operationOnTree : OperationOnTree[A]) {
    lazy val size: Int = operationOnTree.size(t)
    lazy val isEmpty : Boolean = operationOnTree.isEmpty(t)
    lazy val getEmpty : Tree[A] = operationOnTree.getEmpty
    lazy val toList : List[A] = operationOnTree.toList(t)
    def insert(e : A) : Tree[A] = operationOnTree.insert(t)(e)
    def isMember(e : A) : Boolean = operationOnTree.isMember(t)(e)
    def delete(e: A) : Tree[A] = operationOnTree.delete(t)(e)
    def insertAll(els : List[A]) : Tree[A] = operationOnTree.insertAll(t)(els)
  }
  implicit class ToBST[A](t : Tree[A])(implicit bst: BST[A]) extends ToTree[A](t) {
    lazy val getMin : A = bst.getMin(t)
    lazy val getMax : A = bst.getMax(t)
    lazy val isCorrect : Boolean = bst.isCorrect(t)
  }
}

class OperationOnTree[A] {
  def size(t:Tree[A]): Int = t match {
    case Empty => 0
    case Node(sx, _, dx) => size(sx) + size(dx) + 1
  }

  def isEmpty(t:Tree[A]): Boolean = {
    t == Empty
  }

  lazy val getEmpty : Tree[A] = Empty

  def toList(t:Tree[A]): List[A] = {
    def createList(bst: Tree[A], temp: List[A]): List[A] = bst match {
      case Empty => temp
      case Node(sx, el, dx) => createList(sx, el :: createList(dx, temp))
    }

    createList(t, Nil)
  }

  def insert(t:Tree[A])(El: A) : Tree[A] = t match {
    case Empty => Node(Empty, El, Empty)
    case Node(left, value, right) =>
      if(size(left) < size(right)) {
        Node(insert(left)(El), value, right)
      } else {
        Node(left, value, insert(right)(El))
      }
  }

  @tailrec
  final def insertAll(t : Tree[A])(els : List[A]) : Tree[A] = els match {
    case Nil => t
    case x :: xs => insertAll(insert(t)(x))(xs)
  }

  def isMember(t:Tree[A])(El: A): Boolean = {
    t match {
      case Empty => false
      case Node(_, El, _) => true
      case Node(sx, _, dx) =>
        isMember(sx)(El) || isMember(dx)(El)
    }
  }

  def delete(t:Tree[A])(El: A): Tree[A] = t match {
    case Empty => Empty
    case Node(Empty, El, Empty) => Empty
    case Node(sx, El, Empty) => sx
    case Node(Empty, El, dx) => dx
    case Node(sx, El, dx @ Node(_, r, _)) =>
      Node(sx, r, delete(dx)(r))
    case Node(sx, e, dx) =>
      val isMemSx = isMember(sx)(e)
      val isMemDx = isMember(dx)(e)
      if (isMemSx && isMemDx) {
        Node(delete(sx)(El), e, delete(dx)(El))
      } else if (isMemSx) {
        Node(delete(sx)(El), e, dx)
      } else if (isMemDx){
        Node(sx, e, delete(dx)(El))
      } else {
        t
      }
  }
}

class BST[A : Ordering] extends OperationOnTree[A] {
  val ord: Ordering[A] = implicitly

  override final def insert(t: Tree[A])(El: A): Tree[A] = {
    t match {
      case Empty => Node(Empty, El, Empty)
      case Node(_, El, _) => t
      case Node(sx, e, dx) => if (ord.lt(El, e)) {
        Node(insert(sx)(El), e, dx)
      } else {
        Node(sx, e, insert(dx)(El))
      }
    }
  }

  @tailrec
  override final def isMember(t: Tree[A])(El: A): Boolean = {
    t match {
      case Empty => false
      case Node(_, El, _) => true
      case Node(sx, e, dx) => if (ord.lt(El, e)) {
        isMember(sx)(El)
      } else {
        isMember(dx)(El)
      }
    }
  }

  override final def delete(t: Tree[A])(El: A): Tree[A] = {
    t match {
      case Empty => t
      case Node(Empty, El, Empty) => Empty
      case Node(sx, El, Empty) => sx
      case Node(Empty, El, dx) => dx
      case Node(sx, El, dx) =>
        val min: A = getMin(dx)
        Node(sx, min, delete(dx)(min))
      case Node(sx, e, dx) =>
        if (ord.lt(El, e)) {
          Node(delete(sx)(El), e, dx)
        } else {
          Node(sx, e, delete(dx)(El))
        }
    }
  }

  final def isCorrect(t: Tree[A]): Boolean = {
    def check(bst: Tree[A]): Boolean = bst match {
      case Empty => true
      case Node(sx, el, dx) =>
        toList(sx).forall(x =>
          ord.lt(x, el)
        ) && toList(dx).forall(x =>
          ord.gt(x, el)
        ) && check(sx) && check(dx)
    }

    check(t)
  }

  @throws(classOf[NoSuchElementException])
  @tailrec
  final def getMin(t : Tree[A]): A = t match {
    case Empty => throw new NoSuchElementException("Empty.getMin")
    case Node(Empty, e, _) => e
    case Node(sx, _, _) => getMin(sx)
  }

  @throws(classOf[NoSuchElementException])
  @tailrec
  final def getMax(t : Tree[A]): A = t match {
    case Empty => throw new NoSuchElementException("Empty().getMax")
    case Node(_, e, Empty) => e
    case Node(_, _, dx) => getMax(dx)
  }
}