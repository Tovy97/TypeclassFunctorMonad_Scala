import FunctorMain._
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

object FunctorLaws extends Properties("FunctorLaws") {
  property("identity - List EM") = forAll { (a: List[Int]) =>
    a.applyMap((x: Int) => x) == a
  }

  property("identity - List def") = forAll { (a: List[Int]) =>
    applyMap(a)((x: Int) => x) == a
  }

  property("composition - List EM") = forAll { (a: List[Int], f: Int => Int, g: Int => Int) =>
    a.applyMap(f).applyMap(g) == a.applyMap(g compose f)
  }

  property("composition - List def") = forAll { (a: List[Int], f: Int => Int, g: Int => Int) =>
    applyMap(applyMap(a)(f))(g) == applyMap(a)(g compose f)
  }

  property("identity - Option EM") = forAll { (a: Option[Int]) =>
    a.applyMap((x: Int) => x) == a
  }

  property("identity - Option def") = forAll { (a: Option[Int]) =>
    applyMap(a)((x: Int) => x) == a
  }

  property("composition - Option EM") = forAll { (a: Option[Int], f: Int => Int, g: Int => Int) =>
    a.applyMap(f).applyMap(g) == a.applyMap(g compose f)
  }

  property("composition - Option def") = forAll { (a: Option[Int], f: Int => Int, g: Int => Int) =>
    applyMap(applyMap(a)(f))(g) == applyMap(a)(g compose f)
  }
}
