package tree_example.implementation

import tree_example.implementation.TreeImplementation.{BST, OperationOnTree}

object TreeHelper {
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
