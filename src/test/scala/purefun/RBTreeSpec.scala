package purefun

import org.scalacheck._
import org.scalacheck.Prop._

object RBTreeSpec extends Properties("RBTree") {
  property("insert -> member") = forAll { (x: Int, xs: List[Int]) =>
    RBTree.fromSeq(xs).insert(x).member(x)
  }

  property("member ~= Set.contains") = forAll { (b: Byte, bs: Set[Byte]) =>
    RBTree.fromSet(bs).member(b) == bs.contains(b)
  }

  property("size ~= Set.size") = forAll { xs: Set[Int] =>
    RBTree.fromSet(xs).size == xs.size
  }

  property("maximum ~= Set.max") = forAll { xs: Set[Int] =>
    xs.nonEmpty ==> {
      RBTree.fromSet(xs).maximum == Some(xs.max)
    }
  }

  property("minimum ~= Set.min") = forAll { xs: Set[Int] =>
    xs.nonEmpty ==> {
      RBTree.fromSet(xs).minimum == Some(xs.min)
    }
  }
}
