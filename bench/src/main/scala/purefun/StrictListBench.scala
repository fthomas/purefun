package purefun

import org.openjdk.jmh.annotations.{ Benchmark, Scope, State }

@State(Scope.Benchmark)
class StrictListBench {
  val largeList = StrictList.fromSeq(0 to 5000)

  @Benchmark
  def toStringBench: String =
    largeList.toString
}
