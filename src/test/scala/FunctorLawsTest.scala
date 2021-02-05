import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import seminar_codes.FunctorMain.{FunctorLaws, ListFunctor, OptionFunctor, toFunctor1}

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
}
