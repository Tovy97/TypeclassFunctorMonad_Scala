import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import seminar_codes.MonadMain.{ListMonad, ToMonad1, ToMonad2}

object OtherMonadLawsTest extends Properties("OtherMonadLaws") {

  def id[A]: A => A = (x:A) => x
  //LIST
  property("law1") = forAll { (ma : List[Int]) =>
    ma.applyMap(id) == id(ma)
  }
  property("law2") = forAll { (ma : List[Int], f : Int => Int, g : Int => Int) =>
    ma.applyMap(g compose f) == ma.applyMap(f).applyMap(g)
  }

  property("law3") = forAll { (a : Int, f : Int => Int) =>
    ListMonad.unit(a).applyMap(f) == ListMonad.unit(f(a))
  }
  property("law4") = forAll { (mma : List[List[Int]], f : Int => Int) =>
    ListMonad.join(mma).applyMap(f) == ListMonad.join(mma.map(ma => ma.map(f)))
  }

  property("law5") = forAll { (ma : List[Int]) =>
    ListMonad.unit(ma).applyJoin == id(ma)
  }
  property("law6") = forAll { (ma : List[Int]) =>
    ma.applyMap(ListMonad.unit).applyJoin == id(ma)
  }
  property("law7") = forAll { (mmma : List[List[List[Int]]]) =>
    mmma.applyMap(ListMonad.join).applyJoin == mmma.applyJoin.applyJoin
  }
}
