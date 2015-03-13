package purefun

import org.openjdk.jmh.annotations.Benchmark

class RBTreeBench {
  @Benchmark
  def fromSeq: RBTree[Int] =
    RBTree.fromSeq(Seq.range(0, 100))
}
