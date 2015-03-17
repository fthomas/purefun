package purefun

import scala.annotation.tailrec
import scala.util.control.TailCalls._

sealed abstract class StrictList[A] extends Product with Serializable {
  import StrictList._

  final def ++(as: StrictList[A]): StrictList[A] =
    this match {
      case Nil() => as
      case Cons(h, t) => Cons(h, t ++ as)
    }

  final def app2(as: StrictList[A]): StrictList[A] = {
    def go(list: StrictList[A]): TailRec[StrictList[A]] =
      list match {
        case Nil() => done(as)
        case Cons(h, t) => tailcall(go(t).map(t2 => Cons(h, t2)))
      }
    go(this).result
  }

  final def +:(a: A): StrictList[A] =
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

  final def foldRight[B](b: B)(f: (A, B) => B): B =
    reverse.foldLeft(b)((b, a) => f(a, b))

  final def foldRight2[B](b: B)(f: (A, B) => B): B = {
    def go(list: StrictList[A], acc: B): TailRec[B] =
      list match {
        case Nil() => done(acc)
        case Cons(h, t) => tailcall(go(t, acc).map(b => f(h, b)))
      }
    go(this, b).result
  }

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
    (take(n), drop(n))
  }

  final def headOption: Option[A] =
    uncons(None, (h, _) => Some(h))

  final def isEmpty: Boolean =
    uncons(true, (_, _) => false)

  final def map[B](f: A => B): StrictList[B] =
    foldRight(empty[B])((a, l) => Cons(f(a), l))

  final def reverse: StrictList[A] =
    foldLeft(empty[A])((l, a) => Cons(a, l))

  final def size: Int =
    foldLeft(0)((s, _) => s + 1)

  final def tailOption: Option[StrictList[A]] =
    uncons(None, (_, t) => Some(t))

  final def take(n: Int): StrictList[A] = {
    @tailrec
    def go(i: Int, list: StrictList[A], acc: StrictList[A]): StrictList[A] =
      list match {
        case Cons(h, t) if i > 0 => go(i - 1, t, Cons(h, acc))
        case _ => acc.reverse
      }
    go(n, this, empty)
  }

  final def toList: List[A] =
    foldRight(List.empty[A])((a, l) => a +: l)

  final override def toString: String = {
    val name = "StrictList"
    val elems = uncons("", (h, t) => {
      val sb = new StringBuilder(h.toString)
      t.foreach { a => sb.append(", ").append(a.toString); () }
      sb.toString()
    })

    s"$name($elems)"
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
    as.foldRight(empty[A])((a, l) => Cons(a, l))

  def empty[A]: StrictList[A] =
    Nil()

  def fromSeq[A](seq: Seq[A]): StrictList[A] =
    apply(seq: _*)
}
