package purefun

import scala.annotation.tailrec

sealed abstract class LinkedList[A] extends Product with Serializable {
  import LinkedList._

  final def drop(n: Int): LinkedList[A] = {
    def go(i: Int, list: LinkedList[A]): LinkedList[A] =
      if (i <= 0) list
      else list match {
        case Nil() => list
        case Cons(h, t) => go(i - 1, t)
      }
    go(n, this)
  }

  final def flatMap[B](f: A => LinkedList[B]): LinkedList[B] = ???

  final def foldLeft[B](b: B)(f: (B, A) => B): B = {
    @tailrec
    def go(list: LinkedList[A], acc: B): B =
      list match {
        case Nil() => acc
        case Cons(h, t) => go(t, f(acc, h))
      }
    go(this, b)
  }

  final def halve: (LinkedList[A], LinkedList[A]) = ???

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

  final def map[B](f: A => B): LinkedList[B] = {
    @tailrec
    def go(list: LinkedList[A], acc: LinkedList[B]): LinkedList[B] =
      list match {
        case Cons(h, t) => go(t, Cons(f(h), acc))
        case Nil() => acc.reverse
      }
    go(this, empty)
  }

  final def reverse: LinkedList[A] =
    foldLeft(empty[A])((r, a) => Cons(a, r))

  final def size: Int =
    foldLeft(0)((s, _) => s + 1)

  final def tailOption: Option[LinkedList[A]] =
    this match {
      case Nil() => None
      case Cons(h, t) => Some(t)
    }

  final def take(n: Int): LinkedList[A] = {
    @tailrec
    def go(i: Int, list: LinkedList[A], acc: LinkedList[A]): LinkedList[A] =
      if (i <= 0) acc.reverse
      else list match {
        case Nil() => acc.reverse
        case Cons(h, t) => go(i - 1, t, Cons(h, acc))
      }
    go(n, this, empty)
  }
}

object LinkedList {
  final case class Cons[A](head: A, tail: LinkedList[A]) extends LinkedList[A]
  final case class Nil[A]() extends LinkedList[A]

  def apply[A](as: A*): LinkedList[A] =
    as.foldLeft(empty[A])((l, a) => Cons(a, l)).reverse

  def empty[A]: LinkedList[A] = Nil()
}
