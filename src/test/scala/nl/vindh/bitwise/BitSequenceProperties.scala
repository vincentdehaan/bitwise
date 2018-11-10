package nl.vindh.bitwise

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class BitSequenceProperties extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 100)

  "BitSequence" should "implement +" in {
    // Arrange
    forAll {
      tup: (Int, Int) => {
        val x = BitSequence((tup._1 % 256).abs)
        val y = BitSequence((tup._2 % 256).abs)

        // Act
        val z = x + y

        // Assert
        assert(z.toInt === ((tup._1.abs % 256) + (tup._2.abs % 256)) % 256)
      }
    }
  }
}
