package purefun

import org.scalacheck._
import org.scalacheck.Prop._

object StrictListSpec extends Properties("StrictListSpec") {

  implicit def arbitraryStrictList[A](implicit A: Arbitrary[A]): Arbitrary[StrictList[A]] =
    Arbitrary(Gen.listOf(A.arbitrary).map(_.foldLeft(StrictList.empty[A])((l, a) => a :: l)))

  val isEven = (i: Int) => i % 2 == 0

  def maybeIndex(size: Int): Int =
    Gen.choose(-1, size + 1).sample.get

  property("++ ~= List.++") = forAll { (xs: StrictList[Int], ys: StrictList[Int]) =>
    (xs ++ ys).toList == xs.toList ++ ys.toList
  }

  property("drop ~= List.drop") = forAll { xs: StrictList[Int] =>
    val n = maybeIndex(xs.size)
    xs.drop(n).toList == xs.toList.drop(n)
  }

  property("filter ~= List.filter") = forAll { xs: StrictList[Int] =>
    xs.filter(isEven).toList == xs.toList.filter(isEven)
  }

  property("find ~= List.find") = forAll { xs: StrictList[Int] =>
    xs.find(isEven) == xs.toList.find(isEven)
  }

  property("flatMap ~= List.flatMap") = forAll { xs: StrictList[Int] =>
    val f = (i: Int) => StrictList(i - 1, i, i + 1)
    val g = f.andThen(_.toList)
    xs.flatMap(f).toList == xs.toList.flatMap(g)
  }

  property("flatten ~= List.flatten") = forAll { xs: StrictList[StrictList[Int]] =>
    xs.flatten.toList == xs.map(_.toList).toList.flatten
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

  property("lastOption ~= List.lastOption") = forAll { xs: StrictList[Int] =>
    xs.lastOption == xs.toList.lastOption
  }

  property("map ~= List.map") = forAll { xs: StrictList[Int] =>
    val f = (i: Int) => i + 1
    xs.map(f).toList == xs.toList.map(f)
  }

  property("mkString ~= List.mkString") = forAll { xs: StrictList[Int] =>
    val (start, sep, end) = ("(", ",", ")")
    xs.mkString(start, sep, end) == xs.toList.mkString(start, sep, end)
  }

  property("reverse.reverse = id") = forAll { xs: StrictList[Int] =>
    xs.reverse.reverse == xs
  }

  property("splitAt ~= List.splitAt") = forAll { xs: StrictList[Int] =>
    val n = maybeIndex(xs.size)
    val (l, r) = xs.splitAt(n)
    (l.toList, r.toList) == xs.toList.splitAt(n)
  }

  property("size ~= List.size") = forAll { xs: StrictList[Int] =>
    xs.size == xs.toList.size
  }

  property("take ~= List.take") = forAll { xs: StrictList[Int] =>
    val n = maybeIndex(xs.size)
    xs.take(n).toList == xs.toList.take(n)
  }
}
