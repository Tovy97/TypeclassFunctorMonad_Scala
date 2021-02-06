package tree_example.implementation

import scala.annotation.tailrec

object TreeImplementation {

  sealed trait Color

  trait OperationOnTree[A] {
    lazy val getEmpty: Tree[A] = Empty

    def size(t: Tree[A]): Int = t match {
      case Empty => 0
      case Node(sx, _, dx) => size(sx) + size(dx) + 1
    }

    def isEmpty(t: Tree[A]): Boolean = {
      t == Empty
    }

    def toList(t: Tree[A]): List[A] = {
      def createList(bst: Tree[A], temp: List[A]): List[A] = bst match {
        case Empty => temp
        case Node(sx, el, dx) => createList(sx, el :: createList(dx, temp))
      }

      createList(t, Nil)
    }

    def insert(t: Tree[A])(El: A): Tree[A]

    @tailrec
    final def insertAll(t: Tree[A])(els: List[A]): Tree[A] = els match {
      case Nil => t
      case x :: xs => insertAll(insert(t)(x))(xs)
    }

    def isMember(t: Tree[A])(El: A): Boolean = {
      t match {
        case Empty => false
        case Node(_, El, _) => true
        case Node(sx, _, dx) =>
          isMember(sx)(El) || isMember(dx)(El)
      }
    }

    def delete(t: Tree[A])(El: A): Tree[A]
  }

  implicit object IntBST extends BST[Int]

  class BST[A: Ordering] extends OperationOnTree[A] {
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
    final def getMin(t: Tree[A]): A = t match {
      case Empty => throw new NoSuchElementException("Empty.getMin")
      case Node(Empty, e, _) => e
      case Node(sx, _, _) => getMin(sx)
    }

    @throws(classOf[NoSuchElementException])
    @tailrec
    final def getMax(t: Tree[A]): A = t match {
      case Empty => throw new NoSuchElementException("Empty().getMax")
      case Node(_, e, Empty) => e
      case Node(_, _, dx) => getMax(dx)
    }
  }

  class RBT[A: Ordering] extends OperationOnTree[(A, Color)] {
    val ord: Ordering[A] = implicitly

    @tailrec
    override final def isMember(t: Tree[(A, Color)])(elC: (A, Color)): Boolean = {
      val El = elC._1

      t match {
        case Empty => false
        case Node(_, (El, _), _) => true
        case Node(a, (e, _), b) =>
          if (ord.lt(El, e)) {
            isMember(a)((El, Red))
          } else {
            isMember(b)((El, Red))
          }
      }
    }

    final def insertElement(t: Tree[(A, Color)])(el: A): Tree[(A, Color)] = {
      insert(t)((el, Red))
    }

    override final def insert(t: Tree[(A, Color)])(elC: (A, Color)): Tree[(A, Color)] = {
      val El = elC._1

      def ins(rbt: Tree[(A, Color)]): Node[(A, Color)] = rbt match {
        case Empty => Node(Empty, (El, Red), Empty)
        case Node(a, (El, col), b) => Node(a, (El, col), b)
        case Node(a, (y, col), b) =>
          if (ord.lt(El, y)) {
            balance(col, y, ins(a), b)
          } else {
            balance(col, y, a, ins(b))
          }
      }

      val temp = ins(t)
      Node(temp.left, (temp.value._1, Black), temp.right)
    }

    private final def balance(col: Color, el: A, rbt1: Tree[(A, Color)], rbt2: Tree[(A, Color)]): Node[(A, Color)] = {
      (col, el, rbt1, rbt2) match {
        case (Black, z, Node(Node(a, (x, Red), b), (y, Red), c), d) =>
          Node(Node(a, (x, Black), b), (y, Red), Node(c, (z, Black), d))
        case (Black, z, Node(a, (x, Red), Node(b, (y, Red), c)), d) =>
          Node(Node(a, (x, Black), b), (y, Red), Node(c, (z, Black), d))
        case (Black, x, a, Node(b, (y, Red), Node(c, (z, Red), d))) =>
          Node(Node(a, (x, Black), b), (y, Red), Node(c, (z, Black), d))
        case (Black, x, a, Node(Node(b, (y, Red), c), (z, Red), d)) =>
          Node(Node(a, (x, Black), b), (y, Red), Node(c, (z, Black), d))
        case _ => Node(rbt1, (el, col), rbt2)
      }
    }

    final def insertAllElements(t: Tree[(A, Color)])(els: List[A]): Tree[(A, Color)] = {
      insertAll(t)(els.map((_, Red)))
    }

    final def isMemberElement(t: Tree[(A, Color)])(el: A): Boolean = {
      isMember(t)((el, Red))
    }

    final def deleteElement(t: Tree[(A, Color)])(el: A): Tree[(A, Color)] = {
      delete(t)((el, Red))
    }

    override final def delete(t: Tree[(A, Color)])(elC: (A, Color)): Tree[(A, Color)] = {
      val El = elC._1

      def balLeft(rbt1: Tree[(A, Color)], e: A, rbt2: Tree[(A, Color)]): Tree[(A, Color)] =
        (rbt1, e, rbt2) match {
          case (Node(a, (x, Red), b), y, c) => Node(Node(a, (x, Black), b), (y, Red), c)
          case (bl, x, Node(a, (y, Black), b)) => balance(Black, x, bl, Node(a, (y, Red), b))
          case (bl, x, Node(Node(a, (y, Black), b), (z, Red), Node(r, (k, Black), t))) =>
            Node(Node(bl, (x, Black), a), (y, Red), balance(Black, z, b, Node(r, (k, Red), t)))
          case _ => Node(rbt1, (e, Black), rbt2)
        }

      def balRight(rbt1: Tree[(A, Color)], e: A, rbt2: Tree[(A, Color)]): Tree[(A, Color)] =
        (rbt1, e, rbt2) match {
          case (a, x, Node(b, (y, Red), c)) => Node(a, (x, Red), Node(b, (y, Black), c))
          case (Node(a, (x, Black), b), y, bl) => balance(Black, y, Node(a, (x, Red), b), bl)
          case (Node(Node(r, (k, Black), t), (x, Red), Node(b, (y, Black), c)), z, bl) =>
            Node(balance(Black, x, Node(r, (k, Red), t), b), (y, Red), Node(c, (z, Black), bl))
          case _ => Node(rbt1, (e, Black), rbt2)
        }

      def delRight(a: Tree[(A, Color)], y: A, b: Tree[(A, Color)]): Tree[(A, Color)] =
        b match {
          case Node(_, (_, Black), _) => balRight(a, y, del(b))
          case _ => Node(a, (y, Red), del(b))
        }

      def delLeft(a: Tree[(A, Color)], y: A, b: Tree[(A, Color)]): Tree[(A, Color)] =
        a match {
          case Node(_, (_, Black), _) => balLeft(del(a), y, b)
          case _ => Node(del(a), (y, Red), b)
        }

      def merge(rbt1: Tree[(A, Color)], rbt2: Tree[(A, Color)]): Tree[(A, Color)] =
        (rbt1, rbt2) match {
          case (Empty, x) => x
          case (x, Empty) => x
          case (Node(a, (x, Red), b), Node(c, (y, Red), d)) =>
            val k = merge(b, c)
            k match {
              case Node(b1, (z, Red), c1) =>
                Node(Node(a, (x, Red), b1), (z, Red), Node(c1, (y, Red), d))
              case _ => Node(a, (x, Red), Node(k, (y, Red), d))
            }
          case (Node(a, (x, Black), b), Node(c, (y, Black), d)) =>
            val k = merge(b, c)
            k match {
              case Node(b1, (z, Red), c1) =>
                Node(Node(a, (x, Black), b1), (z, Red), Node(c1, (y, Black), d))
              case _ => balLeft(a, x, Node(k, (y, Black), d))
            }
          case (a, Node(b, (x, Red), c)) => Node(merge(a, b), (x, Red), c)
          case (Node(a, (x, Red), b), c) => Node(a, (x, Red), merge(b, c))
        }

      def del(rbt: Tree[(A, Color)]): Tree[(A, Color)] =
        rbt match {
          case Node(a, (El, _), b) => merge(a, b)
          case Node(a, (e, _), b) => if (ord.lt(El, e)) {
            delLeft(a, e, b)
          } else {
            delRight(a, e, b)
          }
          case Empty => rbt
        }

      del(t) match {
        case Empty => Empty
        case Node(sx, (e, _), dx) => Node(sx, (e, Black), dx)
      }
    }

    final def isCorrect(t: Tree[(A, Color)]): Boolean = {
      def checkOrder(rbt: Tree[(A, Color)]): Boolean = rbt match {
        case Empty => true
        case Node(sx, (el, _), dx) =>
          toList(sx).forall(x => ord.lt(x._1, el)) && toList(dx).forall(x => ord.gt(x._1, el)) && checkOrder(sx) && checkOrder(dx)
      }

      def checkRed(rbt: Tree[(A, Color)]): Boolean = rbt match {
        case Empty => true
        case Node(sx, (_, Red), dx) => (sx, dx) match {
          case (Empty, Empty) => true
          case (Node(_, (_, colSx), _), Empty) => colSx == Black && checkRed(sx)
          case (Empty, Node(_, (_, colDx), _)) => colDx == Black && checkRed(dx)
          case (Node(_, (_, colSx), _), Node(_, (_, colDx), _)) => colSx == Black && colDx == Black && checkRed(sx) && checkRed(dx)
        }
        case Node(sx, (_, Black), dx) => checkRed(sx) && checkRed(dx)
      }

      def checkBlack(rbt: Tree[(A, Color)]): (Boolean, Int) = rbt match {
        case Empty => (true, 0)
        case Node(sx, (_, col), dx) =>
          val checkSx = checkBlack(sx)
          val checkDx = checkBlack(dx)
          if (checkSx._1 && checkDx._1 && checkSx._2 == checkDx._2) {
            col match {
              case Red => (true, checkSx._2)
              case Black => (true, checkSx._2 + 1)
            }
          } else {
            (false, -1)
          }
      }

      val root = t match {
        case Empty => true
        case Node(_, (_, col), _) => col == Black
      }
      root && checkOrder(t) && checkRed(t) && checkBlack(t)._1
    }

    @throws(classOf[NoSuchElementException])
    @tailrec
    final def getMin(t: Tree[(A, Color)]): A = {
      t match {
        case Empty => throw new NoSuchElementException("Empty.getMin")
        case Node(Empty, (e, _), _) => e
        case Node(sx, _, _) => getMin(sx)
      }

    }

    @throws(classOf[NoSuchElementException])
    @tailrec
    final def getMax(t: Tree[(A, Color)]): A = {
      t match {
        case Empty => throw new NoSuchElementException("Empty.getMax")
        case Node(_, (e, _), Empty) => e
        case Node(_, _, dx) => getMax(dx)
      }
    }
  }

  case object Red extends Color

  case object Black extends Color

  implicit object IntRBT extends RBT[Int]

}
