package purefun

import scala.annotation.tailrec

sealed abstract class RBTree[A] extends Product with Serializable {
  import RBTree._

  protected def color: Color

  def depth: Int =
    this match {
      case Leaf() => 0
      case Node(l, _, r, _) => 1 + math.max(l.depth, r.depth)
    }

  def depthCPS: Int = {
    def go(tree: RBTree[A], cont: Int => Int): Int =
      tree match {
        case Leaf() => cont(0)
        case Node(l, _, r, _) =>
          go(l, dl =>
            go(r, dr =>
              cont(1 + math.max(dl, dr))))
      }
    go(this, identity)
  }

  def depth2: Int = {
    @tailrec
    def go(d: Int, acc: List[(Int, RBTree[A])]): Int =
      acc match {
        case List() => d
        case (d2, Leaf()) :: xs => go(math.max(d, d2), xs)
        case (d2, Node(l, _, r, _)) :: xs =>
          go(d, (d2 + 1, l) :: (d2 + 1, r) :: xs)
      }
    go(0, List((0, this)))
  }

  final def insert(x: A)(implicit A: Ordering[A]): RBTree[A] = {
    import A._
    def go(tree: RBTree[A]): Node[A] =
      tree match {
        case Leaf() => Node(Leaf(), x, Leaf(), R)
        case node @ Node(l, a, r, c) =>
          if (x < a) Node(go(l), a, r, c).balanceLeft
          else if (x > a) Node(l, a, go(r), c).balanceRight
          else node
      }
    go(this).copy(color = B)
  }

  final def isEmpty: Boolean =
    this match {
      case Leaf() => true
      case _ => false
    }

  final def fold[B](b: B)(f: (B, A, B) => B): B =
    this match {
      case Leaf() => b
      case Node(l, a, r, _) => f(l.fold(b)(f), a, r.fold(b)(f))
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

  @tailrec
  final def maximum: Option[A] =
    this match {
      case Leaf() => None
      case Node(_, a, Leaf(), _) => Some(a)
      case Node(_, _, r, _) => r.maximum
    }

  @tailrec
  final def minimum: Option[A] =
    this match {
      case Leaf() => None
      case Node(Leaf(), a, _, _) => Some(a)
      case Node(l, _, _, _) => l.minimum
    }

  final def size: Int =
    fold(0)((l, _, r) => l + r + 1)
}

object RBTree {
  sealed trait Color
  case object R extends Color
  case object B extends Color

  final case class Leaf[A]() extends RBTree[A] {
    def color: Color = B
  }

  final case class Node[A](left: RBTree[A], elem: A, right: RBTree[A], color: Color) extends RBTree[A] {
    private[purefun] def balanceLeft: Node[A] =
      this match {
        case Node(Node(Node(a, x, b, R), y, c, R), z, d, B) => Node(Node(a, x, b, B), y, Node(c, z, d, B), R)
        case Node(Node(a, x, Node(b, y, c, R), R), z, d, B) => Node(Node(a, x, b, B), y, Node(c, z, d, B), R)
        case _ => this
      }

    private[purefun] def balanceRight: Node[A] =
      this match {
        case Node(a, x, Node(Node(b, y, c, R), z, d, R), B) => Node(Node(a, x, b, B), y, Node(c, z, d, B), R)
        case Node(a, x, Node(b, y, Node(c, z, d, R), R), B) => Node(Node(a, x, b, B), y, Node(c, z, d, B), R)
        case _ => this
      }
  }

  def apply[A: Ordering](as: A*): RBTree[A] =
    as.foldLeft(empty[A])((t, a) => t.insert(a))

  def empty[A]: RBTree[A] =
    Leaf()

  def fromSeq[A: Ordering](as: Seq[A]): RBTree[A] =
    apply(as: _*)

  def fromSet[A: Ordering](as: Set[A]): RBTree[A] =
    as.foldLeft(empty[A])((t, a) => t.insert(a))

  def singleton[A](a: A): RBTree[A] =
    Node(empty, a, empty, B)
}
