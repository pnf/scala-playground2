package gotchas
import java.util.UUID

import org.openjdk.jmh.annotations.Benchmark
import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Benchmark)
class SetMap {


  val set = (1 to 100).toSet
  val list = (1 to 100).toList

  val biggies = (0 to 4).map(_ ⇒ Seq.fill(100)((UUID.randomUUID().toString))).toArray
  def cheapProducerOfLargeResults(i: Int) = biggies(i % 5)


  //  2 * (i * i) faster than 2 * i * i in Java?
  @Benchmark
  def trySet = {
    val res = set.map(cheapProducerOfLargeResults)
    res.size
  }

  @Benchmark
  def tryList = {
    val res = list.map(cheapProducerOfLargeResults)
    res.size
  }
}