package tree_example

import tree_example.implementation.Tree
import tree_example.implementation.TreeImplementation.Color

object TreeMain extends App {
  def RBTMain(): Unit = {
    import tree_example.implementation.TreeHelper.ToRBT
    import tree_example.implementation.TreeImplementation.IntRBT
    val tree: Tree[(Int, Color)] = IntRBT.getEmpty.insertAllElements(List(1, 4, 7, 3, 6, 4, 9, 3, 8, 3, 8, 0, 9, 2))
    println(tree)
    println(tree.isMemberElement(4))
    println(tree.toList)
    println(tree.deleteElement(9))
    println(tree.getMax)
  }

  def BSTMain(): Unit = {
    import tree_example.implementation.TreeHelper.ToBST
    import tree_example.implementation.TreeImplementation.IntBST
    val bst: Tree[Int] = IntBST.getEmpty.insertAll(List(1, 4, 7, 3, 6, 4, 9, 3, 8, 3, 8, 0, 9, 2))
    println(bst)
    println(bst.isMember(4))
    println(bst.toList)
    println(bst.delete(9))
    println(bst.getMax)
  }

  println("IntRBT")
  RBTMain()
  println("\nIntBST")
  BSTMain()
}