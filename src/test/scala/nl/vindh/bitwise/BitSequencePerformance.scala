package nl.vindh.bitwise

import org.scalatest.{FlatSpec, Matchers}

class BitSequencePerformance extends FlatSpec with Matchers {
  def listLessThanOrEqual(lst1: List[Int], lst2: List[Int]): Boolean = lst1.zip(lst2).forall{
    tup => {
      if(tup._1 <= tup._2) true
      else {
        println(tup)
        false
      }
    }
  }

  "BitSequence.+" should "generate at most a specified number of operators" in {
    // Arrange
    val x = BitSequence.variable("x", 8)
    val y = BitSequence.variable("y", 8)
    val z = BitSequence.variable("z", 8)
    val w = BitSequence.variable("w", 8)

    // Act
    val sum = x + y + z + w
    val sumsimple = x.simpleAdd(y).simpleAdd(z).simpleAdd(w)
    val sumsimple2 = x.simpleAdd(y).simpleAdd(z.simpleAdd(w))

    // Assert
    val opCount = sum.bits.map(Metrics.countOperators(_)).map(count => count.ops).toList
    // The values below were observed with a ripple-carry adder adding only two values as implemented in
    // commit 2d749ad184d7df8485358b163b3eab025c354299
    //assert(listLessThanOrEqual(opCount, List(3, 12, 44, 112, 232, 420, 692, 1064)))

    println(sum.bits.map(Metrics.countOperators(_)).mkString("\n"))
    println(sumsimple.bits.map(Metrics.countOperators(_)).mkString("\n"))
    println(sumsimple2.bits.map(Metrics.countOperators(_)).mkString("\n"))
  }
}
