package nl.vindh.bitwise

import org.scalatest._
import types._

class BitSpec extends FlatSpec with Matchers with BitwiseAssertions {
  // Arrange
  val x1 = BitVar("x1")
  val x2 = BitVar("x2")
  val x3 = BitVar("x3")
  val x4 = BitVar("x4")
  val x5 = BitVar("x5")

  "Bit" should "implement &" in {
    // Arrange

    // Act
    val oo = ONE & ONE
    val oz = ONE & ZERO
    val zo = ZERO & ONE
    val zz = ZERO & ZERO
    val of = ONE & x1
    val fo = x1 & ONE
    val zf = ZERO & x1
    val fz = x1 & ZERO
    val ff = x1 & x1

    // Assert
    assert(oo === ONE)
    assert(oz === ZERO)
    assert(zo === ZERO)
    assert(zz === ZERO)
    assert(of === x1)
    assert(fo === x1)
    assert(zf === ZERO)
    assert(fz === ZERO)
    assert(ff === x1)
  }

  it should "implement |" in {
    // Arrange

    // Act
    val oo = ONE | ONE
    val oz = ONE | ZERO
    val zo = ZERO | ONE
    val zz = ZERO | ZERO
    val of = ONE | x1
    val fo = x1 | ONE
    val zf = ZERO | x1
    val fz = x1 | ZERO
    val ff = x1 | x1

    // Assert
    assert(oo === ONE)
    assert(oz === ONE)
    assert(zo === ONE)
    assert(zz === ZERO)
    assert(of === ONE)
    assert(fo === ONE)
    assert(zf === x1)
    assert(fz === x1)
    assert(ff === x1)
  }

  it should "implement ^" in {
    // Arrange

    // Act
    val oo = ONE ^ ONE
    val oz = ONE ^ ZERO
    val zo = ZERO ^ ONE
    val zz = ZERO ^ ZERO
    val of = ONE ^ x1
    val fo = x1 ^ ONE
    val zf = ZERO ^ x1
    val fz = x1 ^ ZERO
    val ff = x1 ^ x1

    // Assert
    assert(oo === ZERO)
    assert(oz === ONE)
    assert(zo === ONE)
    assert(zz === ZERO)
    assert(of === !x1)
    assert(fo === !x1)
    assert(zf === x1)
    assert(fz === x1)
    assert(ff === ZERO)
  }

  it should "implement <->" in {
    // Arrange

    // Act
    val oo = ONE <-> ONE
    val oz = ONE <-> ZERO
    val zo = ZERO <-> ONE
    val zz = ZERO <-> ZERO
    val of = ONE <-> x1
    val fo = x1 <-> ONE
    val zf = ZERO <-> x1
    val fz = x1 <-> ZERO
    val ff = x1 <-> x1
    val fg = x1 <-> x2

    // Assert
    assert(oo === ONE)
    assert(oz === ZERO)
    assert(zo === ZERO)
    assert(zz === ONE)
    assert(of === x1)
    assert(fo === x1)
    assert(zf === !x1)
    assert(fz === !x1)
    assert(ff === ONE)
  }

  it should "implement !" in {
    // Arrange

    // Act
    val o = !ONE
    val z = !ZERO
    val nf = !x1

    // Assert
    assert(o === ZERO)
    assert(z === ONE)
    assert(nf.toString === "(!x1)")
  }

  it should "simplify !!x to x" in {
    // Arrange

    // Act
    val nnx = !(!x1)

    // Assert
    assert(nnx === x1)
  }

  it should "implement onlyAndOrNot" in {
    // Arrange
    val f = (x1 ^ (x2 <-> (x4 ^ x5))) | x3 <-> (x4 ^ !x5)

    // Act
    val andOrNot = f.onlyAndOrNot

    // Assert
    assertEquivalence(f, andOrNot)
    assertAndOrNot(andOrNot)
  }

  it should "implement pushNotInside" in {
    // Arrange
    val f = !(!(!(x1 & x2 | x3 & x4 | !x5)))

    // Act
    val notInside = f.pushNotInside

    // Assert
    assertEquivalence(f, notInside)
    assertNotInside(notInside)
  }

  it should "implement pushOrInside" in {
    // Arrange
    val f = ((x1 & !x2) | (x3 & x4 & (x5| x3)) | x1) | !x4

    // Act
    val orInside = f.pushOrInside

    // Assert
    assertEquivalence(f, orInside)
    assertOrInside(orInside)
  }

  "BitAnd" should "implement substitute" in {
    // Arrange
    val f = x1 & x2
    val m = Map(x1 -> ONE)

    // Act
    val s = f.substitute(m)

    // Assert
    assert(s === x2)
  }

  it should "implement onlyAndOrNot" in {
    // Arrange
    val f = x1 & x2

    // Act
    val andOrNot = f.onlyAndOrNot

    // Assert
    assert(andOrNot === f)
  }

  it should "implement pushNotInside" in {
    // Arrange
    val f = (!(x1 & x2)) & x3

    // Act
    val notInside = f.pushNotInside

    // Assert
    assertEquivalence(f, notInside)
    assertNotInside(notInside)
  }

  it should "implement associateRight" in {
    // Arrange
    val f = ((x1 & x2) & (x3 & x4)) & x5

    // Act
    val right = f.associateRight

    // Assert
    assertEquivalence(f, right)
    assertAssociateRight(right)
  }

  "BitOr" should "implement substitute" in {
    // Arrange
    val f = x1 | x2
    val m = Map(x1 -> ONE)

    // Act
    val s = f.substitute(m)

    // Assert
    assert(s === ONE)
  }

  it should "implement onlyAndOrNot" in {
    // Arrange
    val f = x1 | x2

    // Act
    val andOrNot = f.onlyAndOrNot

    // Assert
    assert(andOrNot === f)
  }

  it should "implement pushNotInside" in {
    // Arrange
    val f = (!(x1 | x2)) | x3

    // Act
    val notInside = f.pushNotInside

    // Assert
    assertEquivalence(f, notInside)
    assertNotInside(notInside)
  }

  it should "implement pushOrInside" in {
    // Arrange
    val f = x1 | (x2 & x3)
    val g = (x1 & x2) | x3

    // Act
    val fOrInside = f.pushOrInside
    val gOrInside = g.pushOrInside

    // Assert
    assertEquivalence(f, fOrInside)
    assertEquivalence(g, gOrInside)
    assertOrInside(fOrInside)
    assertOrInside(gOrInside)
  }

  "BitXor" should "implement substitute" in {
    // Arrange
    val f = x1 ^ x2
    val m = Map(x2 -> ONE)

    // Act
    val s = f.substitute(m)

    // Assert
    assert(s === !x1)
  }

  it should "implement onlyAndOrNot" in {
    // Arrange
    val f = x1 ^ x2

    // Act
    val andOrNot = f.onlyAndOrNot

    // Assert
    assert(andOrNot === ((x1 & !x2) | (!x1 & x2)))
  }

  "BitEq" should "implement substitute" in {
    // Arrange
    val f = x1 <-> x2
    val m = Map(x2 -> ONE)

    // Act
    val s = f.substitute(m)

    // Assert
    assert(s === (x1 <-> ONE))
  }

  it should "implement onlyAndOrNot" in {
    // Arrange
    val f = x1 <-> x2

    // Act
    val andOrNot = f.onlyAndOrNot

    // Assert
    assert(andOrNot === ((!x1 | x2) & (x1 | !x2)))
  }

  "BitNot" should "implement substitute" in {
    // Arrange
    val f = !x1
    val m = Map(x1 -> ONE)

    // Act
    val s = f.substitute(m)

    // Assert
    assert(s === ZERO)
  }

  it should "implement onlyAndOrNot" in {
    // Arrange
    val f = !x1

    // Act
    val andOrNot = f.onlyAndOrNot

    // Assert
    assert(andOrNot === f)
  }

  it should "implement pushNotInside" in {
    // Arrange
    val f = !(x1 & x2)
    val g = !(x1 | x2)

    // Act
    val fNotInside = f.pushNotInside
    val gNotInside = g.pushNotInside

    // Assert
    assertEquivalence(f, fNotInside)
    assertEquivalence(g, gNotInside)
    assertNotInside(fNotInside)
    assertNotInside(gNotInside)
  }

  it should "implement pushOrInside" in {
    // Arrange
    val f = !(x1 | (x2 & x3))

    // Act
    val fOrInside = f.pushOrInside

    // Assert
    assertEquivalence(f, fOrInside)
    assertOrInside(fOrInside)
  }

  "BitVar" should "implement substitute" in {
    // Arrange
    val m = Map(x1 -> ZERO)

    // Act
    val s = x1.substitute(m)

    // Assert
    assert(s === ZERO)
  }

  it should "implement onlyAndOrNot" in {
    // Arrange
    val f = x1

    // Act
    val andOrNot = f.onlyAndOrNot

    // Assert
    assert(andOrNot === f)
  }
}
