package purefun

import org.scalacheck._
import org.scalacheck.Prop._

object StrictListSpec extends Properties("StrictListSpec") {

  implicit def arbitraryStrictList[A](implicit A: Arbitrary[A]): Arbitrary[StrictList[A]] =
    Arbitrary(Gen.listOf(A.arbitrary).map(StrictList.fromSeq))

  def maybeIndex(size: Int): Int =
    Gen.choose(-1, size + 1).sample.get

  property("++ ~= List.++") = forAll { (xs: StrictList[Int], ys: StrictList[Int]) =>
    (xs ++ ys).toList == xs.toList ++ ys.toList
  }

  property("drop ~= List.drop") = forAll { xs: StrictList[Int] =>
    val n = maybeIndex(xs.size)
    xs.drop(n).toList == xs.toList.drop(n)
  }

  property("halve") = forAll { xs: StrictList[Int] =>
    val (l, r) = xs.halve
    val diff = l.size - r.size

    l ++ r == xs && (0 to 1).contains(diff)
  }

  property("headOption ~= List.headOption") = forAll { xs: StrictList[Int] =>
    xs.headOption == xs.toList.headOption
  }

  property("isEmpty ~= List.isEmpty") = forAll { xs: StrictList[Int] =>
    xs.isEmpty == xs.toList.isEmpty
  }

  property("map ~= List.map") = forAll { xs: StrictList[Int] =>
    val f: Int => Int = _ + 1
    xs.map(f).toList == xs.toList.map(f)
  }

  property("reverse.reverse = id") = forAll { xs: StrictList[Int] =>
    xs.reverse.reverse == xs
  }

  property("size ~= List.size") = forAll { xs: StrictList[Int] =>
    xs.size == xs.toList.size
  }

  property("take ~= List.take") = forAll { xs: StrictList[Int] =>
    val n = maybeIndex(xs.size)
    xs.take(n).toList == xs.toList.take(n)
  }
}
