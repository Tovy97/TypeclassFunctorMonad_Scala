import MonadMain._
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

object MonadLaws extends Properties("MonadLaws") {

  //LIST
  property("left identity - List EM") = forAll { (a: Int, f: Int => List[Int]) =>
    List[Int]().unit(a).bind(f) == f(a)
  }

  property("left identity - List def") = forAll { (a: Int, f: Int => List[Int]) =>
    bind(ListMonad.unit(a))(f) == f(a)
  }

  property("right identity - List EM") = forAll { (m: List[Int]) =>
    m.bind(m.unit) == m
  }

  property("right identity - List def") = forAll { (m: List[Int]) =>
    bind(m)(ListMonad.unit) == m
  }

  property("associativity - List EM") = forAll { (m: List[Int], f: Int => List[Int], g: Int => List[Int]) =>
    m.bind(f).bind(g) == m.bind((x: Int) => f(x).bind(g))
  }

  property("associativity - List def") = forAll { (m: List[Int], f: Int => List[Int], g: Int => List[Int]) =>
    bind(bind(m)(f))(g) == bind(m)((x: Int) => bind(f(x))(g))
  }

  //OPTION
  property("left identity - Option EM") = forAll { (a: Int, f: Int => Option[Int]) =>
    (None: Option[Int]).unit(a).bind(f) == f(a)
  }

  property("left identity - Option def") = forAll { (a: Int, f: Int => Option[Int]) =>
    bind(OptionMonad.unit(a))(f) == f(a)
  }

  property("right identity - Option EM") = forAll { (m: Option[Int]) =>
    m.bind(m.unit) == m
  }

  property("right identity - Option def") = forAll { (m: Option[Int]) =>
    bind(m)(OptionMonad.unit) == m
  }

  property("associativity - Option EM") = forAll { (m: Option[Int], f: Int => Option[Int], g: Int => Option[Int]) =>
    m.bind(f).bind(g) == m.bind((x: Int) => f(x).bind(g))
  }

  property("associativity - Option def") = forAll { (m: Option[Int], f: Int => Option[Int], g: Int => Option[Int]) =>
    bind(bind(m)(f))(g) == bind(m)((x: Int) => bind(f(x))(g))
  }
}