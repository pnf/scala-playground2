package add
import org.openjdk.jmh.annotations.Benchmark

import java.util.concurrent.TimeUnit


import org.openjdk.jmh.annotations._

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Benchmark)
class AddTest {

  var i = 10

  //  2 * (i * i) faster than 2 * i * i in Java?
  @Benchmark
  def try2ii = {
      2 * i * i
  }
  @Benchmark
  def try2parenii = {
      2 * (i * i)
  }

}
