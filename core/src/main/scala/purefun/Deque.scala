package purefun

final case class Deque[A] private (front: StrictList[A], rear: StrictList[A]) {
  import Deque._

  def +:(a: A): Deque[A] =
    balance(a :: front, rear)

  def :+(a: A): Deque[A] =
    balance(front, a :: rear)

  def filter(p: A => Boolean): Deque[A] =
    balance(front.filter(p), rear.filter(p))

  def flatMap[B](f: A => Deque[B]): Deque[B] = ???

  def flatten[B](implicit ev: A => Deque[B]): Deque[B] =
    flatMap(ev)

  def foreach(f: A => Unit): Unit = {
    front.foreach(f)
    rear.reverse.foreach(f)
  }

  def headOption: Option[A] =
    front.headOption

  def isEmpty: Boolean =
    front.isEmpty

  def initOption: Option[Deque[A]] =
    rear.uncons(None, (_, t) => Some(balance(front, t)))

  def lastOption: Option[A] =
    rear.headOption.orElse(headOption)

  def map[B](f: A => B): Deque[B] =
    Deque(front.map(f), rear.map(f))

  def tailOption: Option[Deque[A]] =
    front.uncons(None, (_, t) => Some(balance(t, rear)))

  def size: Int =
    front.size + rear.size

  def toList: List[A] = {
    val r = rear.foldLeft(List.empty[A])((l, a) => a :: l)
    front.foldRight(r)((a, l) => a :: l)
  }

  def toStrictList: StrictList[A] =
    front ++ rear.reverse

  override def toString: String = {
    val name = "Deque"
    val elems = {
      val sb = new StringBuilder
      def append(a: A): Unit = { sb.append(", ").append(a.toString); () }

      front.uncons("", (h, t) => {
        sb.append(h.toString)
        t.foreach(append)
        rear.reverse.foreach(append)
        sb.toString()
      })
    }

    s"$name($elems)"
  }
}

object Deque {
  import StrictList._

  def apply[A](as: A*): Deque[A] =
    fromStrictList(StrictList(as: _*))

  private def balance[A](front: StrictList[A], rear: StrictList[A]): Deque[A] =
    (front, rear) match {
      case (Nil(), Nil()) => empty
      case (f, Nil()) =>
        val (nf, nr) = f.halve
        Deque(nf, nr.reverse)
      case (Nil(), r) =>
        val (nr, nf) = r.halve
        Deque(nf.reverse, nr)
      case _ => Deque(front, rear)
    }

  def empty[A]: Deque[A] =
    new Deque[A](StrictList.empty, StrictList.empty)

  def fromSeq[A](as: Seq[A]): Deque[A] =
    fromStrictList(StrictList.fromSeq(as))

  def fromStrictList[A](list: StrictList[A]): Deque[A] =
    balance(list, StrictList.empty)
}
