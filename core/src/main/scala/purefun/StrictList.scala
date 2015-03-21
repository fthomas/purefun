package purefun

import scala.annotation.tailrec

sealed abstract class StrictList[A] extends Product with Serializable {
  import StrictList._

  final def ++(as: StrictList[A]): StrictList[A] =
    if (as.isEmpty) this
    else foldRight(as)((a, l) => a :: l)

  final def ::(a: A): StrictList[A] =
    Cons(a, this)

  @tailrec
  final def drop(n: Int): StrictList[A] =
    this match {
      case Cons(h, t) if n > 0 => t.drop(n - 1)
      case _ => this
    }

  @tailrec
  final def dropWhile(p: A => Boolean): StrictList[A] =
    this match {
      case Cons(h, t) if p(h) => t.dropWhile(p)
      case _ => this
    }

  final def filter(p: A => Boolean): StrictList[A] =
    foldRight(empty[A])((a, l) => if (p(a)) a :: l else l)

  @tailrec
  final def find(p: A => Boolean): Option[A] =
    this match {
      case Cons(h, t) => if (p(h)) Some(h) else t.find(p)
      case _ => None
    }

  final def flatMap[B](f: A => StrictList[B]): StrictList[B] = {
    var result = empty[B]
    foreach { a =>
      f(a).foreach { b =>
        result = b :: result
      }
    }
    result.reverse
  }

  final def flatten[B](implicit ev: A => StrictList[B]): StrictList[B] =
    flatMap(ev)

  final def foldLeft[B](b: B)(f: (B, A) => B): B = {
    @tailrec
    def go(list: StrictList[A], acc: B): B =
      list match {
        case Nil() => acc
        case Cons(h, t) => go(t, f(acc, h))
      }
    go(this, b)
  }

  final def foldRight[B](b: B)(f: (A, B) => B): B =
    reverse.foldLeft(b)((b, a) => f(a, b))

  @tailrec
  final def foreach(f: A => Unit): Unit =
    this match {
      case Nil() => ()
      case Cons(h, t) =>
        f(h)
        t.foreach(f)
    }

  final def halve: (StrictList[A], StrictList[A]) = {
    val n = math.ceil(size / 2.0).toInt
    splitAt(n)
  }

  final def headOption: Option[A] =
    uncons(None, (h, _) => Some(h))

  final def isEmpty: Boolean =
    uncons(true, (_, _) => false)

  @tailrec
  final def lastOption: Option[A] =
    this match {
      case Cons(h, Nil()) => Some(h)
      case Cons(_, t) => t.lastOption
      case Nil() => None
    }

  final def map[B](f: A => B): StrictList[B] =
    foldRight(empty[B])((a, l) => f(a) :: l)

  final def mkString: String =
    mkString("")

  final def mkString(sep: String): String =
    mkString("", sep, "")

  final def mkString(start: String, sep: String, end: String): String = {
    val sb = new StringBuilder(start)
    if (sep.isEmpty) foreach { a => sb.append(a.toString); () }
    else uncons((), (h, t) => {
      sb.append(h.toString)
      t.foreach { a => sb.append(sep).append(a.toString); () }
    })
    sb.append(end)
    sb.result()
  }

  final def reverse: StrictList[A] =
    foldLeft(empty[A])((l, a) => a :: l)

  final def size: Int =
    foldLeft(0)((s, _) => s + 1)

  final def splitAt(n: Int): (StrictList[A], StrictList[A]) = {
    @tailrec
    def go(front: StrictList[A], n: Int, rear: StrictList[A]): (StrictList[A], StrictList[A]) =
      rear match {
        case Cons(h, t) if n > 0 => go(h :: front, n - 1, t)
        case _ => (front.reverse, rear)
      }
    go(empty, n, this)
  }

  final def tailOption: Option[StrictList[A]] =
    uncons(None, (_, t) => Some(t))

  final def take(n: Int): StrictList[A] = {
    @tailrec
    def go(i: Int, list: StrictList[A], acc: StrictList[A]): StrictList[A] =
      list match {
        case Cons(h, t) if i > 0 => go(i - 1, t, h :: acc)
        case _ => acc.reverse
      }
    go(n, this, empty)
  }

  final def toList: List[A] =
    foldRight(List.empty[A])((a, l) => a +: l)

  final override def toString: String = {
    val prefix = "StrictList"
    mkString(s"$prefix(", ", ", ")")
  }

  final def uncons[B](b: => B, f: (A, StrictList[A]) => B): B =
    this match {
      case Nil() => b
      case Cons(h, t) => f(h, t)
    }
}

object StrictList {
  final case class Cons[A](head: A, tail: StrictList[A]) extends StrictList[A]
  final case class Nil[A]() extends StrictList[A]

  def apply[A](as: A*): StrictList[A] =
    as.foldRight(empty[A])((a, l) => a :: l)

  def empty[A]: StrictList[A] =
    Nil()

  def fromSeq[A](seq: Seq[A]): StrictList[A] =
    apply(seq: _*)
}
