package nl.vindh.bitwise

import org.scalatest._

class BitSequenceSpec extends FlatSpec with Matchers with BitVarXs {
  "BitSequence" should "generate a BitSequence" in {
    // Arrange

    // Act
    val bs0 = BitSequence(0)
    val bs1 = BitSequence(1)
    val bs17 = BitSequence(17)

    // Assert
    assert(bs0.toInt === 0)
    assert(bs1.toInt === 1)
    assert(bs17.toInt === 17)
  }

  it should "generate a BitSequence from a hex string" in {
    // Arrange

    // Act
    val bsff = BitSequence("FF")
    val bscafe = BitSequence("CAFE")
    val bslong = BitSequence("AAAAAAAAAAAAAAAAAAAAAAAAAA")

    // Assert
    assert(bsff.toInt === 255)
    assert(bscafe.toInt === 51966)
    assert(bslong.bits.size === 26*4)
  }

  it should "generate a BitSequence from an ASCII string" in {
    // Arrange

    // Act
    val abc = BitSequence.fromAscii("abc")

    // Assert
    assert(abc === BitSequence.fromSeq(
      List(
        ZERO, ONE, ONE, ZERO, ZERO, ZERO, ZERO, ONE, // "a"
        ZERO, ONE, ONE, ZERO, ZERO, ZERO, ONE, ZERO, // "b"
        ZERO, ONE, ONE, ZERO, ZERO, ZERO, ONE, ONE // "c"
      )))
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
    val xs0 = BitSequence(0)
    val xs1 = BitSequence(1)
    val xs23 = BitSequence(23)
    val xs45 = BitSequence(45)

    // Act
    val s23_45 = xs23 + xs45
    val s0_1 = xs0 + xs1

    // Assert
    assert(s23_45.toInt === 68)
    assert(s23_45.length === WORD_SIZE)
    assert(s0_1.toInt === 1)
    assert(s0_1.length === WORD_SIZE)
  }

  it should "implement variable" in {
    // Arrange

    // Act
    val xs = BitSequence.variable("x", 4)

    // Assert
    // TODO: this test gives stack overflow error; seems to be the same problem as my research project
    //assert(xs === new BitSequence(Array(x0, x1, x2, x3)))
  }

  it should "implement substitute" in {
    // Arrange
    val xs = BitSequence.variable("x", 4)
    val m = Map(x1 -> ZERO, x2 -> ONE)

    // Act
    val s = xs.substitute(m)

    // Assert
    // TODO: idem
    //assert(s === new BitSequence(List(x0, ZERO, ONE, x3)))
  }

  it should "behave like a normal collection" in {
    // Arrange
    val bs1 = BitSequence(12, 4)
    val bs2 = BitSequence(13, 4)

    // Act
    val concat = bs1 ++ bs2

    // Assert
    // TODO: fix this
    //assert(concat.getClass == bs1.getClass)
  }

  // TODO
  /*it should "implement concatenation with ||" in {
    // Arrange
    val bs1 = BitSequence(12, 4)
    val bs2 = BitSequence(13, 4)

    // Act
    val concat = bs1 || bs2

    // Assert
    assert(concat == BitSequence(195, 8))
  }*/

  "BitSequence.empty" should "return a sequence of length 0" in {
    // Arrange
    val e = BitSequence.empty
    val f = BitSequence(12)

    // Act
    val g = e || f

    // Assert
    assert(e.bits.length === 0)
    assert(g.bits.length == f.bits.length)
    assert(f === g)
  }
}