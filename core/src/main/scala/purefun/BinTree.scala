package purefun

sealed abstract class BinTree[A] extends Product with Serializable {

}

object BinTree {
  def empty[A]: BinTree[A] = ???
}
