import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import seminar_codes.implementation.functor.FunctorImplementation._
import seminar_codes.implementation.functor.FunctorLaws
import tree_example.implementation.TreeImplementation.Color

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
  property("identity - RBT") = forAll { (fa: List[Int]) =>
    import tree_example.implementation.TreeHelper.ToRBT
    import tree_example.implementation.TreeImplementation.IntRBT
    val t = IntRBT.getEmpty.insertAllElements(fa)
    FunctorLaws()(TreeFunctor).identity(t)
  }

  property("composition - Tree") = forAll { (fa: List[Int], fT: Int => Int, gT: Int => Int) =>
    import tree_example.implementation.TreeHelper.ToRBT
    import tree_example.implementation.TreeImplementation.IntRBT
    val t = IntRBT.getEmpty.insertAllElements(fa)
    val f = (vc: (Int, Color)) => {
      val (x, c) = vc
      (fT(x), c)
    }
    val g = (vc: (Int, Color)) => {
      val (x, c) = vc
      (gT(x), c)
    }
    FunctorLaws()(TreeFunctor).composition(t)(f, g)
  }

  //BST
  property("identity - BST") = forAll { (fa: List[Int]) =>
    import tree_example.implementation.TreeHelper.ToBST
    import tree_example.implementation.TreeImplementation.IntBST
    val t = IntBST.getEmpty.insertAll(fa)
    FunctorLaws()(TreeFunctor).identity(t)
  }

  property("composition - BST") = forAll { (fa: List[Int], f: Int => Int, g: Int => Int) =>
    import tree_example.implementation.TreeHelper.ToBST
    import tree_example.implementation.TreeImplementation.IntBST
    val t = IntBST.getEmpty.insertAll(fa)
    FunctorLaws()(TreeFunctor).composition(t)(f, g)
  }
}
