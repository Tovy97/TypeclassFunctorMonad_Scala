import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import seminar_codes.implementation.monad.MonadImplementation._
import seminar_codes.implementation.monad.MonadLaws
import wadler_example.IdExample.Id
import wadler_example.ExceptionExample.RaiseReturn
import wadler_example.IOExample.IOStr
//import wadler_example.StateExample.{IntState, IntStateMonad}

object MonadLawsTest extends Properties("MonadLaws") {

  //LIST
  property("left identity - List") = forAll { (a: Int, f: Int => List[Int]) =>
    MonadLaws()(ListMonad).leftIdentity(a)(f)
  }

  property("right identity - List") = forAll { (m: List[Int]) =>
    MonadLaws()(ListMonad).rightIdentity(m)
  }

  property("associativity - List") = forAll { (m: List[Int], f: Int => List[Int], g: Int => List[Int]) =>
    MonadLaws()(ListMonad).associativity(m)(f, g)
  }

  property("identity - List") = forAll { (ma: List[Int]) =>
    MonadLaws()(ListMonad).identity(ma)
  }

  property("composition - List") = forAll { (ma: List[Int], f: Int => Int, g: Int => Int) =>
    MonadLaws()(ListMonad).composition(ma)(f, g)
  }

  property("bind = map + join - List") = forAll { (ma : List[Int], f: Int=>List[Int]) =>
    MonadLaws()(ListMonad).bindMapJoin(ma)(f)
  }

  //OPTION
  property("left identity - Option") = forAll { (a: Int, f: Int => Option[Int]) =>
    MonadLaws()(OptionMonad).leftIdentity(a)(f)
  }

  property("right identity - Option") = forAll { (m: Option[Int]) =>
    MonadLaws()(OptionMonad).rightIdentity(m)
  }

  property("associativity - Option") = forAll { (m: Option[Int], f: Int => Option[Int], g: Int => Option[Int]) =>
    MonadLaws()(OptionMonad).associativity(m)(f, g)
  }

  property("identity - Option") = forAll { (ma: Option[Int]) =>
    MonadLaws()(OptionMonad).identity(ma)
  }

  property("composition - Option") = forAll { (ma: Option[Int], f: Int => Int, g: Int => Int) =>
    MonadLaws()(OptionMonad).composition(ma)(f, g)
  }

  property("bind = map + join - Option") = forAll { (ma : Option[Int], f: Int=>Option[Int]) =>
    MonadLaws()(OptionMonad).bindMapJoin(ma)(f)
  }

  //ID
  property("left identity - Id") = forAll { (a: Int, fT: Int => Int) =>
    def f(x : Int) : Id[Int] = IdentityMonad.unit(fT(x))
    MonadLaws()(IdentityMonad).leftIdentity(a)(f)
  }

  property("right identity - Id") = forAll { (mT: Int) =>
    val m = IdentityMonad.unit(mT)
    MonadLaws()(IdentityMonad).rightIdentity(m)
  }

  property("associativity - Id") = forAll { (mT: Int, fT: Int => Int, gT: Int => Int) =>
    def f(x : Int) : Id[Int] = IdentityMonad.unit(fT(x))
    def g(x : Int) : Id[Int] = IdentityMonad.unit(gT(x))
    val m = IdentityMonad.unit(mT)
    MonadLaws()(IdentityMonad).associativity(m)(f, g)
  }

  property("identity - Id") = forAll { (mT: Int) =>
    val m = IdentityMonad.unit(mT)
    MonadLaws()(IdentityMonad).identity(m)
  }

  property("composition - Id") = forAll { (mT: Int, f: Int => Int, g: Int => Int) =>
    val m = IdentityMonad.unit(mT)
    MonadLaws()(IdentityMonad).composition(m)(f, g)
  }

  property("bind = map + join - Id") = forAll { (mT: Int, fT: Int => Int) =>
    def f(x : Int) : Id[Int] = IdentityMonad.unit(fT(x))
    val m = IdentityMonad.unit(mT)
    MonadLaws()(IdentityMonad).bindMapJoin(m)(f)
  }

  //EXCEPTION
  property("left identity - Exception") = forAll { (a: Int, fT: Int => Int) =>
    def f(x : Int) : RaiseReturn[Int] = RaiseReturnMonad.unit(fT(x))
    MonadLaws()(RaiseReturnMonad).leftIdentity(a)(f)
  }

  property("right identity - Exception") = forAll { (mT: Int) =>
    val m = RaiseReturnMonad.unit(mT)
    MonadLaws()(RaiseReturnMonad).rightIdentity(m)
  }

  property("associativity - Exception") = forAll { (mT: Int, fT: Int => Int, gT: Int => Int) =>
    def f(x : Int) : RaiseReturn[Int] = RaiseReturnMonad.unit(fT(x))
    def g(x : Int) : RaiseReturn[Int] = RaiseReturnMonad.unit(gT(x))
    val m = RaiseReturnMonad.unit(mT)
    MonadLaws()(RaiseReturnMonad).associativity(m)(f, g)
  }

  property("identity - Exception") = forAll { (mT: Int) =>
    val m = RaiseReturnMonad.unit(mT)
    MonadLaws()(RaiseReturnMonad).identity(m)
  }

  property("composition - Exception") = forAll { (mT: Int, f: Int => Int, g: Int => Int) =>
    val m = RaiseReturnMonad.unit(mT)
    MonadLaws()(RaiseReturnMonad).composition(m)(f, g)
  }

  property("bind = map + join - Exception") = forAll { (mT: Int, fT: Int => Int) =>
    def f(x : Int) : RaiseReturn[Int] = RaiseReturnMonad.unit(fT(x))
    val m = RaiseReturnMonad.unit(mT)
    MonadLaws()(RaiseReturnMonad).bindMapJoin(m)(f)
  }

  //STATE - Tests fail due to undecidability of function equality
  /*
  property("left identity - State") = forAll { (a: Int, fT: Int => Int) =>
    def f(x : Int) : IntState[Int] = IntStateMonad.unit(fT(x))
    MonadLaws()(IntStateMonad).leftIdentity(a)(f)
  }

  property("right identity - State") = forAll { (mT: Int) =>
    val m = IntStateMonad.unit(mT)
    MonadLaws()(IntStateMonad).rightIdentity(m)
  }

  property("associativity - State") = forAll { (mT: Int, fT: Int => Int, gT: Int => Int) =>
    def f(x : Int) : IntState[Int] = IntStateMonad.unit(fT(x))
    def g(x : Int) : IntState[Int] = IntStateMonad.unit(gT(x))
    val m = IntStateMonad.unit(mT)
    MonadLaws()(IntStateMonad).associativity(m)(f, g)
  }

  property("identity - State") = forAll { (mT: Int) =>
    val m = IntStateMonad.unit(mT)
    MonadLaws()(IntStateMonad).identity(m)
  }

  property("composition - State") = forAll { (mT: Int, f: Int => Int, g: Int => Int) =>
    val m = IntStateMonad.unit(mT)
    MonadLaws()(IntStateMonad).composition(m)(f, g)
  }

  property("bind = map + join - State") = forAll { (mT: Int, fT: Int => Int) =>
    def f(x : Int) : IntState[Int] = IntStateMonad.unit(fT(x))
    val m = IntStateMonad.unit(mT)
    MonadLaws()(IntStateMonad).bindMapJoin(m)(f)
  }
  */

  //IO
  property("left identity - IO") = forAll { (a: Int, fT: Int => Int) =>
    def f(x : Int) : IOStr[Int] = IOStrMonad.unit(fT(x))
    MonadLaws()(IOStrMonad).leftIdentity(a)(f)
  }

  property("right identity - IO") = forAll { (mT: Int) =>
    val m = IOStrMonad.unit(mT)
    MonadLaws()(IOStrMonad).rightIdentity(m)
  }

  property("associativity - IO") = forAll { (mT: Int, fT: Int => Int, gT: Int => Int) =>
    def f(x : Int) : IOStr[Int] = IOStrMonad.unit(fT(x))
    def g(x : Int) : IOStr[Int] = IOStrMonad.unit(gT(x))
    val m = IOStrMonad.unit(mT)
    MonadLaws()(IOStrMonad).associativity(m)(f, g)
  }

  property("identity - IO") = forAll { (mT: Int) =>
    val m = IOStrMonad.unit(mT)
    MonadLaws()(IOStrMonad).identity(m)
  }

  property("composition - IO") = forAll { (mT: Int, f: Int => Int, g: Int => Int) =>
    val m = IOStrMonad.unit(mT)
    MonadLaws()(IOStrMonad).composition(m)(f, g)
  }

  property("bind = map + join - IO") = forAll { (mT: Int, fT: Int => Int) =>
    def f(x : Int) = IOStrMonad.unit(fT(x))
    val m = IOStrMonad.unit(mT)
    MonadLaws()(IOStrMonad).bindMapJoin(m)(f)
  }

}
