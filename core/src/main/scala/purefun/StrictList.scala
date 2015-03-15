package purefun

import scala.annotation.tailrec

sealed abstract class StrictList[A] extends Product with Serializable {
  import StrictList._

  final def ++(as: StrictList[A]): StrictList[A] = ???

  final def +:(a: A): StrictList[A] =
    Cons(a, this)

  final def drop(n: Int): StrictList[A] = {
    def go(i: Int, list: StrictList[A]): StrictList[A] =
      if (i <= 0) list
      else list match {
        case Nil() => list
        case Cons(h, t) => go(i - 1, t)
      }
    go(n, this)
  }

  final def flatMap[B](f: A => StrictList[B]): StrictList[B] = ???

  final def foldLeft[B](b: B)(f: (B, A) => B): B = {
    @tailrec
    def go(list: StrictList[A], acc: B): B =
      list match {
        case Nil() => acc
        case Cons(h, t) => go(t, f(acc, h))
      }
    go(this, b)
  }

  final def halve: (StrictList[A], StrictList[A]) = ???

  final def headOption: Option[A] =
    this match {
      case Cons(h, _) => Some(h)
      case Nil() => None
    }

  final def isEmpty: Boolean =
    this match {
      case Nil() => true
      case _ => false
    }

  final def map[B](f: A => B): StrictList[B] =
    foldLeft(empty[B])((l, a) => Cons(f(a), l)).reverse

  final def reverse: StrictList[A] =
    foldLeft(empty[A])((l, a) => Cons(a, l))

  final def size: Int =
    foldLeft(0)((s, _) => s + 1)

  final def tailOption: Option[StrictList[A]] =
    this match {
      case Nil() => None
      case Cons(h, t) => Some(t)
    }

  final def take(n: Int): StrictList[A] = {
    @tailrec
    def go(i: Int, list: StrictList[A], acc: StrictList[A]): StrictList[A] =
      if (i <= 0) acc.reverse
      else list match {
        case Nil() => acc.reverse
        case Cons(h, t) => go(i - 1, t, Cons(h, acc))
      }
    go(n, this, empty)
  }

  final override def toString: String =
    this match {
      case Nil() => "StrictList()"
      case Cons(h, t) => "StrictList(" + h + t.foldLeft("")((s, a) => s + ", " + a) + ")"
    }
}

object StrictList {
  final case class Cons[A](head: A, tail: StrictList[A]) extends StrictList[A]
  final case class Nil[A]() extends StrictList[A]

  def apply[A](as: A*): StrictList[A] =
    as.foldLeft(empty[A])((l, a) => Cons(a, l)).reverse

  def empty[A]: StrictList[A] =
    Nil()

  def fromSeq[A](seq: Seq[A]): StrictList[A] =
    apply(seq: _*)
}
