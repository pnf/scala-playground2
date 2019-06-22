package gotchas

import java.util.UUID
import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.{Benchmark, _}

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Benchmark)
class DeepMap {


  val vector = (1 to 100).toVector
  val list = (1 to 100).toList
  val array = list.toArray

  @Benchmark
  def nestedList = {
    var res = 0
    for(
      i ← list;
      j ← list;
      k ← list)
      res += i + j + k
    res
  }

  @Benchmark
  def nestedVector = {
    var res = 0
    for(
      i ← vector;
      j ← vector;
      k ← vector)
      res += i + j + k
    res
  }

  @Benchmark
  def whileLoopVector = {
    var res = 0
    var i = 0
    while(i < 100) {
      var j = 0
      while(j < 100) {
        var k = 0
        while(k < 100) {
          res += vector(i) + vector(j) + vector(k)
          k += 1
        }
        j += 1
      }
      i += 1
    }
    res
  }

  @Benchmark
  def whileLoopArray= {
    var res = 0
    var i = 0
    while(i < 100) {
      var j = 0
      while(j < 100) {
        var k = 0
        while(k < 100) {
          res += array(i) + array(j) + array(k)
          k += 1
        }
        j += 1
      }
      i += 1
    }
    res
  }

  @Benchmark
  def whileLoopList = {
    var res = 0
    var i = list
    while(!i.isEmpty) {
      var j =list
      while(!j.isEmpty) {
        var k = list
        while(!k.isEmpty) {
          res += i.head + j.head + k.head
          k = k.tail
        }
        j = j.tail
      }
      i = i.tail
    }
    res
  }

  @Benchmark
  def whileLoopListToArray= {
    var res = 0
    var i = 0
    val ai = list.toArray
    while(i < 100) {
      var j = 0
      val aj = list.toArray
      while(j < 100) {
        var k = 0
        val ak = list.toArray
        while(k < 100) {
          res += ai(i) + aj(j) + ak(k)
          k += 1
        }
        j += 1
      }
      i += 1
    }
    res
  }


}
