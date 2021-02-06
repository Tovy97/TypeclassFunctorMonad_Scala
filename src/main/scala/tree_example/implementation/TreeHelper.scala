package tree_example.implementation

import tree_example.implementation.TreeImplementation.{BST, Color, OperationOnTree, RBT}

object TreeHelper {

  implicit class ToTree[A](t: Tree[A])(implicit operationOnTree: OperationOnTree[A]) {
    lazy val size: Int = operationOnTree.size(t)
    lazy val isEmpty: Boolean = operationOnTree.isEmpty(t)
    lazy val getEmpty: Tree[A] = operationOnTree.getEmpty
    lazy val toList: List[A] = operationOnTree.toList(t)

    def insert(e: A): Tree[A] = operationOnTree.insert(t)(e)

    def isMember(e: A): Boolean = operationOnTree.isMember(t)(e)

    def delete(e: A): Tree[A] = operationOnTree.delete(t)(e)

    def insertAll(els: List[A]): Tree[A] = operationOnTree.insertAll(t)(els)
  }

  implicit class ToBST[A](t: Tree[A])(implicit bst: BST[A]) extends ToTree[A](t) {
    lazy val getMin: A = bst.getMin(t)
    lazy val getMax: A = bst.getMax(t)
    lazy val isCorrect: Boolean = bst.isCorrect(t)
  }

  implicit class ToRBT[A](t: Tree[(A, Color)])(implicit rbt: RBT[A]) extends ToTree[(A, Color)](t) {
    lazy val getMin: A = rbt.getMin(t)
    lazy val getMax: A = rbt.getMax(t)
    lazy val isCorrect: Boolean = rbt.isCorrect(t)

    def insertElement(e: A): Tree[(A, Color)] = rbt.insertElement(t)(e)

    def isMemberElement(e: A): Boolean = rbt.isMemberElement(t)(e)

    def deleteElement(e: A): Tree[(A, Color)] = rbt.deleteElement(t)(e)

    def insertAllElements(els: List[A]): Tree[(A, Color)] = rbt.insertAllElements(t)(els)
  }

}
