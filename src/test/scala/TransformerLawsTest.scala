import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import seminar_codes.TransformerMain.{ListOptionT, TransformerLaws}

object TransformerLawsTest extends Properties("TransformerLaws") {

  //ListOption
  property("left identity - ListOption") = forAll { (a: Int, f: Int => List[Option[Int]]) =>
    TransformerLaws().leftIdentity(a)(f)
  }

  property("right identity - ListOption") = forAll { (t: List[Option[Int]]) =>
    TransformerLaws().rightIdentity(t)
  }

  property("associativity - ListOption") = forAll { (t: List[Option[Int]], f: Int => List[Option[Int]], g: Int => List[Option[Int]]) =>
    TransformerLaws().associativity(t)(f, g)
  }

  property("identity - ListOption") = forAll { (t: List[Option[Int]]) =>
    TransformerLaws().identity(t)
  }

  property("composition - ListOption") = forAll { (t: List[Option[Int]], f: Int => Int, g: Int => Int) =>
    TransformerLaws().composition(t)(f, g)
  }

  property("bind = map + join - ListOption") = forAll { (t : List[Option[Int]], f: Int=>List[Option[Int]]) =>
    TransformerLaws().bindMapJoin(t)(f)
  }
}