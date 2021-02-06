package tree_example

import tree_example.implementation.Tree

object TreeMain extends App {
  def genericTreeMain(): Unit = {
    import tree_example.implementation.TreeImplementation.IntTree
    import tree_example.implementation.TreeHelper.ToTree
    val tree : Tree[Int] = IntTree.getEmpty.insertAll(List(1,4,7,3,6,4,9,3,8,3,8,0,9,2))
    println(tree)
    println(tree.isMember(4))
    println(tree.toList)
    println(tree.delete(9))
    //println(tree.getMax) //Available only on BST
  }
  def BSTMain() : Unit = {
    import tree_example.implementation.TreeImplementation.IntBST
    import tree_example.implementation.TreeHelper.ToBST
    val bst : Tree[Int] = IntBST.getEmpty.insertAll(List(1,4,7,3,6,4,9,3,8,3,8,0,9,2))
    println(bst)
    println(bst.isMember(4))
    println(bst.toList)
    println(bst.delete(9))
    println(bst.getMax)
  }

  println("IntTree")
  genericTreeMain()
  println("\nIntBST")
  BSTMain()
}