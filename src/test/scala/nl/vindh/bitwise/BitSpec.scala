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
    val fg = x1 & x2

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
    assert(fg.toString === "(x1&x2)")
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
    val fg = x1 | x2

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
    assert(fg.toString === "(x1|x2)")
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
    val fg = x1 ^ x2

    // Assert
    assert(oo === ZERO)
    assert(oz === ONE)
    assert(zo === ONE)
    assert(zz === ZERO)
    assert(of.toString === "(1^x1)")
    assert(fo.toString === "(x1^1)")
    assert(zf.toString === "(0^x1)")
    assert(fz.toString === "(x1^0)")
    assert(ff === ZERO)
    assert(fg.toString === "(x1^x2)")
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
    assert(of.toString === "(1==x1)")
    assert(fo.toString === "(x1==1)")
    assert(zf.toString === "(0==x1)")
    assert(fz.toString === "(x1==0)")
    assert(ff === ONE)
    assert(fg.toString === "(x1==x2)")
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

    assertEquivalence(f, andOrNot, List(x1, x2, x3, x4, x5))
    assert(!andOrNot.toString.contains('^'))
    assert(!andOrNot.toString.contains('='))
  }

  it should "implement pushNotInside" in {
    // Arrange
    val f = !(!(!(x1 & x2 | x3 & x4 | !x5)))

    // Act
    val notInside = f.pushNotInside

    assertEquivalence(f, notInside, List(x1, x2, x3, x4, x5))
    assertTree(notInside){
      _ match {
        case BitNot(not) => not.isInstanceOf[BitVar]
        case _ => true
      }
    }
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

  "BitXor" should "implement substitute" in {
    // Arrange
    val f = x1 ^ x2
    val m = Map(x2 -> ONE)

    // Act
    val s = f.substitute(m)

    // Assert
    assert(s.toString === "(x1^1)")
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
    assert(s.toString === "(x1==1)")
  }

  it should "implement onlyAndOrNot" in {
    // Arrange
    val f = x1 <-> x2

    // Act
    val andOrNot = f.onlyAndOrNot

    // Assert
    assert(andOrNot === ((x1 & x2) | (!x1 & !x2)))
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
