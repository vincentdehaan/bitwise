package nl.vindh.bitwise

import org.scalatest._

class BitSequenceSpec extends FlatSpec with Matchers {
  // Arrange
  val x1 = BitVar("x1")
  val x2 = BitVar("x2")
  val x3 = BitVar("x3")
  val x4 = BitVar("x4")
  val x5 = BitVar("x5")

  "BitSequence" should "generate a BitSequence" in {
    // Arrange

    // Act
    val bs = BitSequence(17)

    // Assert
    assert(bs.toInt === 17)
  }

  it should "implement &" in {
    // Arrange
    val xs = BitSequence(107)
    val ys = BitSequence(221)

    // Act
    val and = xs & ys

    // Assert
    assert(and.toInt === 73)
  }

  it should "implement |" in {
    // Arrange
    val xs = BitSequence(14)
    val ys = BitSequence(29)

    // Act
    val or = xs | ys

    // Assert
    assert(or.toInt === 31)
  }

  it should "implement ^" in {
    // Arrange
    val xs = BitSequence(14)
    val ys = BitSequence(29)

    // Act
    val xor = xs ^ ys

    // Assert
    assert(xor.toInt === 19)
  }

  it should "implement eq" in {
    // Arrange
    val xs = BitSequence(14, 8) //Note that the answer depends on the word size
    val ys = BitSequence(29, 8)

    // Act
    val eq = xs <-> ys

    // Assert
    assert(eq.toInt === 236)
  }

  it should "implement !" in {
    // Arrange
    val xs = BitSequence(17, 8) // Note that the answer depends on the word size

    // Act
    val not = !xs

    // Assert
    assert(not.toInt == 238)
  }

  it should "implement >>>" in {
    // Arrange
    val xs = BitSequence(23, 8) // Note that the answer depends on the word size

    // Act
    val rot = xs >>> 2

    // Assert
    assert(rot.toInt === 197)
  }

  it should "implement +" in {
    // Arrange
    val xs = BitSequence(23)
    val ys = BitSequence(45)

    // Act
    val sum = xs + ys

    // Assert
    assert(sum.toInt === 68)
    assert(sum.length === xs.length)
  }

  it should "implement variable" in {
    // Arrange

    // Act
    val xs = BitSequence.variable("x", 4)

    // Assert
    assert(xs.toString === "(x0,x1,x2,x3)")
  }

  it should "implement substitute" in {
    // Arrange
    val xs = BitSequence.variable("x", 4)
    val m = Map(x1 -> ZERO, x2 -> ONE)

    // Act
    val s = xs.substitute(m)

    // Assert
    assert(s.toString === "(x0,0,1,x3)")
  }
}