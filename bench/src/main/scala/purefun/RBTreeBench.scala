package purefun

import org.openjdk.jmh.annotations.{ Benchmark, Scope, State }

@State(Scope.Benchmark)
class RBTreeBench {
  val seq: Seq[Int] = Seq(1, 2, 3).flatMap(i => i to 200 by 3)

  @Benchmark
  def fromSeq: RBTree[Int] =
    RBTree.fromSeq(seq)
}
