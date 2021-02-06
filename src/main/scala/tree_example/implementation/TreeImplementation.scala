package tree_example.implementation

import scala.annotation.tailrec

object TreeImplementation {
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
        val isMemSx = isMember(sx)(El)
        val isMemDx = isMember(dx)(El)
        if (isMemSx) {
          Node(delete(sx)(El), e, dx)
        } else if (isMemDx){
          Node(sx, e, delete(dx)(El))
        } else {
          t
        }
    }
  }
  implicit object IntTree extends OperationOnTree[Int]

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
  implicit object IntBST extends BST[Int]
}
