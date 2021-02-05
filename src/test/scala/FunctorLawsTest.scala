import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import seminar_codes.FunctorMain.{FunctorLaws, ListFunctor, OptionFunctor}
import tree_example.TreeMain.TreeFunctor

object FunctorLawsTest extends Properties("FunctorLaws") {
  //LIST
  property("identity - List") = forAll { (fa: List[Int]) =>
    FunctorLaws()(ListFunctor).identity(fa)
  }

  property("composition - List") = forAll { (fa: List[Int], f: Int => Int, g: Int => Int) =>
    FunctorLaws()(ListFunctor).composition(fa)(f, g)
  }

  //OPTION
  property("identity - Option") = forAll { (fa: Option[Int]) =>
    FunctorLaws()(OptionFunctor).identity(fa)
  }

  property("composition - Option") = forAll { (fa: Option[Int], f: Int => Int, g: Int => Int) =>
    FunctorLaws()(OptionFunctor).composition(fa)(f, g)
  }

  //Tree
  property("identity - Tree") = forAll { (fa: List[Int]) =>
    import tree_example.TreeMain.IntTree
    import tree_example.Converter.ToTree
    val t = IntTree.getEmpty.insertAll(fa)
    FunctorLaws()(TreeFunctor).identity(t)
  }

  property("composition - Tree") = forAll { (fa: List[Int], f: Int => Int, g: Int => Int) =>
    import tree_example.TreeMain.IntTree
    import tree_example.Converter.ToTree
    val t = IntTree.getEmpty.insertAll(fa)
    FunctorLaws()(TreeFunctor).composition(t)(f, g)
  }

  //BST
  property("identity - BST") = forAll { (fa: List[Int]) =>
    import tree_example.TreeMain.IntBST
    import tree_example.Converter.ToBST
    val t = IntBST.getEmpty.insertAll(fa)
    FunctorLaws()(TreeFunctor).identity(t)
  }

  property("composition - BST") = forAll { (fa: List[Int], f: Int => Int, g: Int => Int) =>
    import tree_example.TreeMain.IntBST
    import tree_example.Converter.ToBST
    val t = IntBST.getEmpty.insertAll(fa)
    FunctorLaws()(TreeFunctor).composition(t)(f, g)
  }
}
