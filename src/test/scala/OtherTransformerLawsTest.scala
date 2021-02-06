import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import seminar_codes.implementation.transformer.TransformerHelper._
import seminar_codes.implementation.transformer.TransformerImplementation.ListOptionT

object OtherTransformerLawsTest extends Properties("OtherTransformerLaws") {

  def id[A]: A => A = (x: A) => x

  property("law1") = forAll { (t: List[Option[Int]]) =>
    t.applyMapT(id) == id(t)
  }
  property("law2") = forAll { (t: List[Option[Int]], f: Int => Int, g: Int => Int) =>
    t.applyMapT(g compose f) == t.applyMapT(f).applyMapT(g)
  }

  property("law3") = forAll { (ma: Option[Int], f: Option[Int] => Option[Int]) =>
    ListOptionT.unit(ma).applyMapT(f) == ListOptionT.unit(f(ma))
  }
  property("law4") = forAll { (mma: List[Option[List[Option[Int]]]], f: Int => Int) =>
    mma.applyJoinT.applyMapT(f) == mma.applyMapT(ma => ma.applyMapT(f)).applyJoinT
  }

  property("law5") = forAll { (ma: List[Option[Int]]) =>
    ListOptionT.unit(ma).applyJoinT == id(ma)
  }
  property("law6") = forAll { (ma: List[Option[Int]]) =>
    ma.applyMapT(ListOptionT.unit).applyJoinT == id(ma)
  }
  property("law7") = forAll { (mmma: List[Option[List[Option[List[Option[Int]]]]]]) =>
    mmma.applyMapT(ListOptionT.join).applyJoinT == mmma.applyJoinT.applyJoinT
  }
}


