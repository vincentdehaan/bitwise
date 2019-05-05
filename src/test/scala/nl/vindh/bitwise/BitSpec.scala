package nl.vindh.bitwise

import org.scalatest._

class BitSpec extends FlatSpec with Matchers with BitwiseAssertions with BitVarXs {
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
    //val ff = x1 ^ x1 // TODO: decide if this is efficient to implement

    // Assert
    assert(oo === ZERO)
    assert(oz === ONE)
    assert(zo === ONE)
    assert(zz === ZERO)
    assert(of === !x1)
    assert(fo === !x1)
    assert(zf === x1)
    assert(fz === x1)
    //assert(ff === ZERO)
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
    val f1 = (x1 ^ (x2 <-> (x4 ^ x5))) | x3 <-> (x4 ^ !x5)
    val f2 = x1 ^ x2

    // Act
    val andOrNot1 = f1.onlyAndOrNot
    val andOrNot2 = f2.onlyAndOrNot

    // Assert
    assertEquivalence(f1, andOrNot1)
    assertEquivalence(f2, andOrNot2)
    assertAndOrNot(andOrNot1)
    assertAndOrNot(andOrNot2)
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
    val f1 = ((x1 & !x2) | (x3 & x4 & (x5| x3)) | x1) | !x4
    val f2 = x1 | (x2 & x3)
    val f3 = (x1 | (x2 & x3)) & x4

    // Act
    val orInside1 = f1.pushOrInside
    val orInside2 = f2.pushOrInside
    val orInside3 = f3.pushOrInside

    // Assert
    assertEquivalence(f1, orInside1)
    assertOrInside(orInside1)
    assertEquivalence(f2, orInside2)
    assertOrInside(orInside2)
    assertEquivalence(f3, orInside3)
    assertOrInside(orInside3)
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
    assertEquivalence(andOrNot, f)
  }

  it should "implement pushNotInside" in {
    // Arrange
    val f = (!(x1 & x2)) & x3

    // Act
    val notInside = f.pushNotInside

    // Assert
    assertEquivalence(notInside, f)
    assertNotInside(notInside)
  }

  it should "implement x & !x == ZERO" in {
    // Arrange

    // Act
    val f1 = x1 & !x1
    val f2 = x1 & x2 & x3 & !x1

    // Assert
    assert(f1 === ZERO)
    assert(f2 === ZERO)
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
    assertEquivalence(andOrNot, f)
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

  it should "implement x | !x === ONE" in {
    // Arrange

    // Act
    val f1 = x1 | !x1
    val f2 = x1 | x2 | x3 | !x1

    // Assert
    assert(f1 === ONE)
    assert(f2 === ONE)
  }

  "BitXor" should "implement substitute" in {
    // Arrange
    val f = x1 ^ x2
    val m = Map(x2 -> ONE)

    // Act
    val s = f.substitute(m)

    // Assert
    assert(s === (x1 ^ ONE))
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
    val f1 = !(x1 | (x2 & x3))
    val f2 = x1|(x3&((!x5)|(x7&(!x6))|((!x7)&x6))&(x5|(((!x7)|x6)&(x7|(!x6)))))
    val f3 = x1|((!x3)&((x5&((!x7)|x6)&(x7|(!x6)))|((!x5)&((x7&(!x6))|((!x7)&x6)))))
    val f4 = x1 | (x2 & x3) | (x4 & x5)
    val f5 = ((!x1)|(x3&((!x5)|(x7&(!x6))|x1)&(x5|x1)|((!x3)&(x2|((!x5)&x1)))))

    // Act
    val f1OrInside = f1.pushOrInside
    val f2OrInside = f2.pushOrInside
    val f3OrInside = f3.pushOrInside
    val f4OrInside = f4.pushOrInside
    val f5OrInside = f5.pushOrInside

    // Assert
    assertEquivalence(f1, f1OrInside)
    assertOrInside(f1OrInside)
    assertEquivalence(f2, f2OrInside)
    assertOrInside(f2OrInside)
    assertEquivalence(f3, f3OrInside)
    assertOrInside(f3OrInside)
    assertEquivalence(f4, f4OrInside)
    assertOrInside(f4OrInside)
    assertEquivalence(f5, f5OrInside)
    assertOrInside(f5OrInside)
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
