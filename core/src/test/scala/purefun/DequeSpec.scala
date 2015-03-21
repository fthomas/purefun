package purefun

import org.scalacheck.Prop._
import org.scalacheck.{ Arbitrary, Gen, Properties }

object DequeSpec extends Properties("DequeSpec") {

  implicit def arbitraryDeque[A](implicit A: Arbitrary[A]): Arbitrary[Deque[A]] =
    Arbitrary(Gen.listOf(A.arbitrary).map(Deque.fromSeq))

  property("+: -> headOption") = forAll { (x: Int, xs: Deque[Int]) =>
    (x +: xs).headOption == Some(x)
  }

  property(":+ -> lastOption") = forAll { (x: Int, xs: Deque[Int]) =>
    (xs :+ x).lastOption == Some(x)
  }

  property("headOption ~= List.headOption") = forAll { xs: Deque[Int] =>
    xs.headOption == xs.toList.headOption
  }

  property("isEmpty ~= List.isEmpty") = forAll { xs: Deque[Int] =>
    xs.isEmpty == xs.toList.isEmpty
  }

  property("lastOption ~= List.lastOption") = forAll { xs: Deque[Int] =>
    xs.lastOption == xs.toList.lastOption
  }

  property("map ~= List.map") = forAll { xs: Deque[Int] =>
    val f = (i: Int) => i + 1
    xs.map(f).toList == xs.toList.map(f)
  }

  property("size ~= List.size") = forAll { xs: Deque[Int] =>
    xs.size == xs.toList.size
  }
}
