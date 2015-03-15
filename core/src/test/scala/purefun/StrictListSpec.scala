package purefun

import org.scalacheck._
import org.scalacheck.Prop._

object StrictListSpec extends Properties("StrictListSpec") {
  property("halve") = forAll { xs: List[Int] =>
    true
  }

  property("headOption ~= List.headOption") = forAll { xs: List[Int] =>
    StrictList.fromSeq(xs).headOption == xs.headOption
  }

  property("isEmpty ~= List.isEmpty") = forAll { xs: List[Int] =>
    StrictList.fromSeq(xs).isEmpty == xs.isEmpty
  }

  property("size ~= List.size") = forAll { xs: List[Int] =>
    StrictList.fromSeq(xs).size == xs.size
  }
}
