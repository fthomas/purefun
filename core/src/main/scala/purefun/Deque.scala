package purefun

final class Deque[A] private (front: StrictList[A], rear: StrictList[A]) {
  import Deque._
  import StrictList._

  def +:(a: A): Deque[A] =
    balance(a :: front, rear)

  def :+(a: A): Deque[A] =
    balance(front, a :: rear)

  def foreach(f: A => Unit): Unit = {
    front.foreach(f)
    rear.reverse.foreach(f)
  }

  def headOption: Option[A] =
    front.headOption

  def initOption: Option[Deque[A]] =
    rear match {
      case Cons(h, t) => Some(balance(front, t))
      case _ => None
    }

  def lastOption: Option[A] =
    rear.headOption

  def map[B](f: A => B): Deque[B] =
    new Deque(front.map(f), rear.map(f))

  def tailOption: Option[Deque[A]] =
    front match {
      case Cons(h, t) => Some(balance(t, rear))
      case _ => None
    }

  def toList: List[A] = {
    val r = rear.foldLeft(List.empty[A])((l, a) => a :: l)
    front.foldRight(r)((a, l) => a :: l)
  }

  def toStrictList: StrictList[A] =
    front ++ rear.reverse
}

object Deque {
  import StrictList._

  def apply[A](as: A*): Deque[A] = ???

  private def balance[A](front: StrictList[A], rear: StrictList[A]): Deque[A] =
    (front, rear) match {
      case (Nil(), Nil()) => empty
      case (f, Nil()) =>
        val (nf, nr) = f.halve
        new Deque(nf, nr.reverse)
      case (Nil(), r) =>
        val (nr, nf) = r.halve
        new Deque(nf.reverse, nr)
      case _ => new Deque(front, rear)
    }

  def empty[A]: Deque[A] =
    new Deque[A](StrictList.empty, StrictList.empty)

  def fromSeq[A](as: Seq[A]): Deque[A] = ???

  def fromStrictList[A](list: StrictList[A]): Deque[A] =
    balance(list, StrictList.empty)
}
