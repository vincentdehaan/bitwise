package nl.vindh.bitwise

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import org.scalacheck.Gen

class BitSequenceProperties extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 100)

  "BitSequence" should "implement +" in {
    // Arrange
    forAll {
      tup: (Int, Int) => {
        val x = BitSequence((tup._1.abs % 256))
        val y = BitSequence((tup._2.abs % 256))

        // Act
        val z = x + y

        // Assert
        assert(z.toInt === (((tup._1.abs % 256) + (tup._2.abs % 256)) % 256))
      }
    }
  }

  "BitSequence" should "implement +/" in {
    // Arrange
    forAll {
      tup: (Int, Int) => {
        val x = BitSequence(tup._1.abs % 256)
        val y = BitSequence(tup._2.abs % 256)
        implicit val vargen = new VariableGenerator("t")

        // Act
        val (s, d) = x +/ y

        // Assert
        assert(s.substitute(d).toInt === (((tup._1.abs % 256) + (tup._2.abs % 256)) % 256))
      }
    }
  }

  it should "implement >>>" in {
    // Arrange
    val len = 32
    val shift = 7
    val gen = Gen.containerOfN[List, BitValue](len, Gen.oneOf(ZERO, ONE))

    // TODO: if this test fails, ScalaCheck tries to shrink the test vector. However, it does not understand that it needs to be of fixed length. How do I fix this?
    forAll(gen) {
      lst: List[BitValue] => {
        val bs = BitSequence.fromSeq(lst)

        // Act
        val sh = bs >>> shift

        // Assert
        (0 until len).foreach {
          i => assert(sh((i + shift) % len) === bs(i))
        }
      }
    }
  }

  it should "implement <<<" in {
    // Arrange
    val len = 32
    val shift = 7
    val gen = Gen.containerOfN[List, BitValue](len, Gen.oneOf(ZERO, ONE))

    // TODO: if this test fails, ScalaCheck tries to shrink the test vector. However, it does not understand that it needs to be of fixed length. How do I fix this?
    forAll(gen) {
      lst: List[BitValue] => {
        val bs = BitSequence.fromSeq(lst)

        // Act
        val sh = bs <<< shift

        // Assert
        (0 until len).foreach {
          i => assert(sh((i - shift + len) % len) === bs(i))
        }
      }
    }
  }
}
