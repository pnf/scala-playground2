package gotchas

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.{Benchmark, _}

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Benchmark)
class Collect {

  val as = (1 to 1000).toList
  val abs: List[((Int, Int), Int)] = as.zip(as).zip(as).toList

  @Benchmark
  def collect = as.collect {
    case i if i % 2 == 0 ⇒ i + 1
  }

  @Benchmark
  def flatMop = as.flatMap {
    case i if i%2 == 0 ⇒ Some(i)
    case _ => None
  }

  @Benchmark
  def filter = as.filter(_ % 2 == 0).map(_ + 1)

  @Benchmark
  def unapply = abs.map {
    case ((a, b), c) ⇒ a + b + c
  }

  @Benchmark
  def underply = abs.map(x ⇒ x._1._1 + x._1._2 + x._2)


}
