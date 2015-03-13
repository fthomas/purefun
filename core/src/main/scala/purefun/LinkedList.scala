package purefun

import scala.annotation.tailrec

sealed abstract class LinkedList[A] extends Product with Serializable {
  def flatMap[B](f: A => LinkedList[B]): LinkedList[B] = ???

  def headOption: Option[A] =
    this match {
      case Cons(head, _) => Some(head)
      case Nil() => None
    }

  def map[B](f: A => B): LinkedList[B] = {
    @tailrec
    def go(list: LinkedList[A], acc: LinkedList[B]): LinkedList[B] =
      list match {
        case Cons(head, tail) => go(tail, Cons(f(head), acc))
        case Nil() => acc.reverse
      }
    go(this, Nil())
  }

  def reverse: LinkedList[A] = {
    @tailrec
    def go(list: LinkedList[A], acc: LinkedList[A]): LinkedList[A] =
      list match {
        case Cons(head, tail) => go(tail, Cons(head, acc))
        case Nil() => acc
      }
    go(this, Nil())
  }
}

case class Cons[A](head: A, tail: LinkedList[A]) extends LinkedList[A]
case class Nil[A]() extends LinkedList[A]

object LinkedList {
  def apply[A](seq: A*): LinkedList[A] = ???

  def empty[A]: LinkedList[A] = Nil()
}
