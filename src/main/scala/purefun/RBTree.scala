package purefun

import scala.annotation.tailrec

sealed trait Color
case object R extends Color
case object B extends Color

sealed abstract class RBTree[A] extends Product with Serializable {
  protected def color: Color

  def insert(x: A)(implicit A: Ordering[A]): RBTree[A] = {
    import A._
    def go(tree: RBTree[A]): Node[A] =
      tree match {
        case Leaf() => Node(Leaf(), x, Leaf(), R)
        case node @ Node(l, a, r, c) =>
          if (x < a) Node(go(l), a, r, c).balance
          else if (x > a) Node(l, a, go(r), c).balance
          else node
      }
    go(this).copy(color = B)
  }

  @tailrec
  final def member(x: A)(implicit A: Ordering[A]): Boolean = {
    import A._
    this match {
      case Leaf() => false
      case Node(l, a, r, _) =>
        if (x < a) l.member(x)
        else if (x > a) r.member(x)
        else true
    }
  }
}

case class Leaf[A]() extends RBTree[A] {
  def color: Color = B
}

case class Node[A](left: RBTree[A], elem: A, right: RBTree[A], color: Color) extends RBTree[A] {
  private[purefun] def balance: Node[A] =
    this match {
      case Node(Node(Node(a, x, b, R), y, c, R), z, d, B) => Node(Node(a, x, b, B), y, Node(c, z, d, B), R)
      case Node(Node(a, x, Node(b, y, c, R), R), z, d, B) => Node(Node(a, x, b, B), y, Node(c, z, d, B), R)
      case Node(a, x, Node(Node(b, y, c, R), z, d, R), B) => Node(Node(a, x, b, B), y, Node(c, z, d, B), R)
      case Node(a, x, Node(b, y, Node(c, z, d, R), R), B) => Node(Node(a, x, b, B), y, Node(c, z, d, B), R)
      case _ => this
    }
}

object RBTree {
  def apply[A: Ordering](as: A*): RBTree[A] =
    as.foldLeft(empty[A])((t, a) => t.insert(a))

  def empty[A]: RBTree[A] = Leaf()

  def singleton[A](a: A): RBTree[A] = Node(empty, a, empty, B)
}
