package nl.vindh.bitwise

import org.scalatest._
import types._

class BitSpec extends FlatSpec with Matchers {
  "The Bit class" should "implement &" in {// TODO: formula arguments
    // Arrange

    // Act
    val oo = ONE & ONE
    val oz = ONE & ZERO
    val zo = ZERO & ONE
    val zz = ZERO & ZERO

    // Assert
    assert(oo === ONE)
    assert(oz === ZERO)
    assert(zo === ZERO)
    assert(zz === ZERO)
  }

  it should "implement |" in {// TODO: formula arguments
    // Arrange

    // Act
    val oo = ONE | ONE
    val oz = ONE | ZERO
    val zo = ZERO | ONE
    val zz = ZERO | ZERO

    // Assert
    assert(oo === ONE)
    assert(oz === ONE)
    assert(zo === ONE)
    assert(zz === ZERO)
  }

  it should "implement ^" in {// TODO: formula arguments
    // Arrange

    // Act
    val oo = ONE ^ ONE
    val oz = ONE ^ ZERO
    val zo = ZERO ^ ONE
    val zz = ZERO ^ ZERO

    // Assert
    assert(oo === ZERO)
    assert(oz === ONE)
    assert(zo === ONE)
    assert(zz === ZERO)
  }

  it should "implement !" in {// TODO: formula arguments
    // Arrange

    // Act
    val o = !ONE
    val z = !ZERO

    // Assert
    assert(o === ZERO)
    assert(z === ONE)
  }


}
