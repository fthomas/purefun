package purefun

class Deque[A](private val front: StrictList[A], private val rear: StrictList[A]) {
  import Deque._
  import StrictList._

  private def balance: Deque[A] =
    (front, rear) match {
      case (Nil(), Nil()) => this
      case (f, Nil()) => fromStrictList(f)
      case (Nil(), r) =>
        val (nr, nf) = r.halve
        new Deque(nf.reverse, nr)
      case _ => this
    }

  final def headOption: Option[A] =
    front.headOption

  final def initOption: Option[Deque[A]] =
    rear match {
      case Cons(h, t) => Some(new Deque(front, t).balance)
      case _ => None
    }

  final def lastOption: Option[A] =
    rear.headOption

  final def tailOption: Option[Deque[A]] =
    front match {
      case Cons(h, t) => Some(new Deque(t, rear).balance)
      case _ => None
    }
}

object Deque {
  def empty[A]: Deque[A] =
    new Deque[A](StrictList.empty, StrictList.empty)

  def fromSeq[A](as: Seq[A]): Deque[A] = ???

  def fromStrictList[A](list: StrictList[A]): Deque[A] = {
    val (front, rear) = list.halve
    new Deque[A](front, rear.reverse)
  }
}
