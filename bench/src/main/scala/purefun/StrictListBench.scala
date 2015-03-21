package purefun

import org.openjdk.jmh.annotations.{ Benchmark, Scope, State }

@State(Scope.Benchmark)
class StrictListBench {
  val largeList = StrictList.fromSeq(0 to 50000)

  @Benchmark
  def halve = largeList.toString
}
