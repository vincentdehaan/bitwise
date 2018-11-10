package nl.vindh.bitwise

import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck._

class BitPropertiesSlow extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks with BitwiseAssertions
  with BitVarXs with ArbitraryBits {
  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 1)

  "Bit" should "implement onlyAndOrNot" in {
    // Arrange
    forAll {
      f: Bit => {
        // Act
        val andOrNot = f.onlyAndOrNot

        // Assert
        assertEquivalence(f, andOrNot)
        assertAndOrNot(andOrNot)
      }
    }
  }

  it should "implement pushNotInside" in {
    // Arrange
    forAll {
      f: Bit => {
        val andOrNot = f.onlyAndOrNot // pushNotInside is only defined on these formulas

        // Act
        val notInside = andOrNot.pushNotInside

        // Assert
        assertEquivalence(andOrNot, notInside)
        assertNotInside(notInside)
      }
    }
  }

  it should "implement pushOrInside" in {
    // Arrange
    forAll {
      f: Bit => {
        val andOrNot = f.onlyAndOrNot
        val notInside = andOrNot.pushNotInside

        // Act
        val orInside = notInside.pushOrInside

        // Assert
        assertEquivalence(notInside, orInside)
        assertOrInside(orInside)
      }
    }
  }
}
