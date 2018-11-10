package nl.vindh.bitwise

import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck._

class BitProperties extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks with BitwiseAssertions
  with BitVarXs with ArbitraryBits {
  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 1000)

  it should "implement &" in {
    // Arrange
    forAll {
      tup: (Bit, Bit) => {
        // Act
        val f = tup._1 & tup._2

        // Assert
        assertBinaryOperator(f, tup._1, tup._2, _ & _)
      }
    }
  }

  it should "implement |" in {
    // Arrange
    forAll {
      tup: (Bit, Bit) => {
        // Act
        val f = tup._1 | tup._2

        // Assert
        assertBinaryOperator(f, tup._1, tup._2, _ | _)
      }
    }
  }

  it should "implement !" in {
    // Arrange
    forAll {
      bit: Bit => {
        // Act
        val f = !bit

        // Assert
        assertBinaryOperator(f, ONE, bit, _ ^ _)
      }
    }
  }
}
