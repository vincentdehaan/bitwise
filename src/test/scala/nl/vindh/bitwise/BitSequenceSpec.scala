package nl.vindh.bitwise

import org.scalatest._

class BitSequenceSpec extends FlatSpec with Matchers with BitVarXs {
  "BitSequence" should "generate a BitSequence" in {
    // Arrange

    // Act
    val bs0 = BitSequence(0)
    val bs1 = BitSequence(1)
    val bs17 = BitSequence(17)
    val bs218 = BitSequence(218)
    val bs7long = BitSequence(7, 64)

    // Assert
    assert(bs0.toInt === 0)
    assert(bs0.bits.size === WORD_SIZE)
    assert(bs1.toInt === 1)
    assert(bs1.bits.size === WORD_SIZE)
    assert(bs17.toInt === 17)
    assert(bs17.bits.size === WORD_SIZE)
    assert(bs218.toInt === 218)
    assert(bs218.bits.size === WORD_SIZE)
    assert(bs7long.toInt === 7)
    assert(bs7long.bits.size === 64)
  }

  it should "generate a BitSequence from a hex string" in {
    // Arrange

    // Act
    val bsff = BitSequence("FF")
    val bscafe = BitSequence("CAFE")
    val bslong = BitSequence("AAAAAAAAAAAAAAAAAAAAAAAAAA")

    // Assert
    assert(bsff.toHexString === "ff")
    assert(bscafe.toHexString === "cafe")
    assert(bslong.bits.size === 26*4)
  }

  it should "implement &" in {
    // Arrange
    val xs1 = BitSequence(107)
    val ys1 = BitSequence(221)
    val xs2 = BitSequence("a8f109ff")
    val ys2 = BitSequence("cde596f9")

    // Act
    val and1 = xs1 & ys1
    val and2 = xs2 & ys2

    // Assert
    assert(and1.toInt === 73)
    assert(and2.toHexString === "88e100f9")
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
    val xs1 = BitSequence(14)
    val ys1 = BitSequence(29)
    val xs2 = BitSequence("410c420e")
    val ys2 = BitSequence("10021071")

    // Act
    val xor1 = xs1 ^ ys1
    val xor2 = xs2 ^ ys2

    // Assert
    assert(xor1.toInt === 19)
    assert(xor2.toHexString === "510e527f")
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
    val xs1 = BitSequence(17, 8) // Note that the answer depends on the word size
    val xs2 = BitSequence("e12d4f0e")

    // Act
    val not1 = !xs1
    val not2 = !xs2

    // Assert
    assert(not1.toInt == 238)
    assert(not2.toHexString === "1ed2b0f1")
  }

  it should "implement >>>" in {
    // Arrange
    val xs = BitSequence(23, 8) // Note that the answer depends on the word size
    val xshex = BitSequence("123456")

    // Act
    val rot = xs >>> 2
    val rothex = xshex >>> 8

    // Assert
    assert(rot.toInt === 92)
    assert(rothex.toHexString === "345612")
  }

  it should "implement >>" in {
    // Arrange
    val xs1 = BitSequence(23, 8)
    val xs2 = BitSequence("6f20776f")

    // Act
    val sh1 = xs1 >> 2
    val sh2 = xs2 >> 3

    // Assert
    assert(sh1.toInt == 5)
    assert(sh2.toHexString === "0de40eed")
  }

  it should "implement <<<" in {
    // Arrange
    val xs1 = BitSequence("446df4b9")

    // Act
    val sh1 = xs1 <<< 13

    // Assert
    assert(sh1.toHexString === "a5ca236f")
  }

  it should "implement +" in {
    // Arrange
    val xs0 = BitSequence(0)
    val xs1 = BitSequence(1)
    val xs23 = BitSequence(23)
    val xs45 = BitSequence(45)
    val xs2 = BitSequence("a54ff53a")
    val xs3 = BitSequence("3bdd59d4")

    // Act
    val s23_45 = xs23 + xs45
    val s0_1 = xs0 + xs1
    val s2_3 = xs2 + xs3

    // Assert
    assert(s23_45.toInt === 68)
    assert(s23_45.length === WORD_SIZE)
    assert(s0_1.toInt === 1)
    assert(s0_1.length === WORD_SIZE)
    assert(s2_3.toHexString === "e12d4f0e")
  }

  it should "implement toHexString" in {
    // Arrange
    val bs23 = BitSequence("17")
    val bs1203 = BitSequence("1203")
    val bs258 = BitSequence("102")
    val bs0258 = BitSequence("0258")

    // Act
    val s23 = bs23.toHexString
    val s1203 = bs1203.toHexString
    val s258 = bs258.toHexString
    val s0258 = bs0258.toHexString

    // Asset
    assert(s23 === "17")
    assert(s1203 === "1203")
    assert(s258 === "0102")
    assert(s0258 === "0258")
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

  "fromAscii" should "convert a single ASCII character correctly" in {
    // Arrange

    // Act
    val a = BitSequence.fromAscii("a")

    // Assert
    assert(a.toInt == 97)
  }

  it should "generate a BitSequence from an ASCII string" in {
    // Arrange

    // Act
    val abc = BitSequence.fromAscii("abc")

    // Assert
    //assert(abc === BitSequence.fromSeq(
    //  List(
    //    ONE, ZERO, ZERO, ZERO, ZERO, ONE, ONE, ZERO, // "a"
    //    ZERO, ONE, ZERO, ZERO, ZERO, ONE, ONE, ZERO, // "b"
    //    ONE, ONE, ZERO, ZERO, ZERO, ONE, ONE, ZERO // "c"
    //  )))
  }

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