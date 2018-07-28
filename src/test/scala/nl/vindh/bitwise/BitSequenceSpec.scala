package nl.vindh.bitwise

import org.scalatest._
import types._

class BitSequenceSpec extends FlatSpec with Matchers {
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
    val xs = BitSequence(23, 8) // Note that the answer depents on the word size

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
}