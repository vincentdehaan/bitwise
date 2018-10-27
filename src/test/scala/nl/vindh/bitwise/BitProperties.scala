package nl.vindh.bitwise

// TODO: cleanup
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck._
import types._

class BitProperties extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks with BitwiseAssertions {
  // TODO: create seperate trait/file
  val x1 = BitVar("x1")
  val x2 = BitVar("x2")
  val x3 = BitVar("x3")
  val x4 = BitVar("x4")
  val x5 = BitVar("x5")
  val x6 = BitVar("x6")
  val x7 = BitVar("x7")
  val x8 = BitVar("x8")

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 1)

  implicit def arbitraryBit: Arbitrary[Bit] = Arbitrary {
    val primitiveGen = Gen.oneOf(ZERO, ONE)
    val varGen = Gen.oneOf(x1, x2, x3, x4, x5, x6, x7, x8)
    def notGen(level: Int): Gen[Bit] = for{
      bit <- bitGen(level - 1)
    } yield (! bit)

    def binGen(level: Int): Gen[Bit] = for {
      left <- bitGen(level - 1)
      right <- bitGen(level - 1)
      op <- Gen.oneOf(
        (_:Bit) & (_:Bit),
        (_:Bit) | (_:Bit),
        (_:Bit) <-> (_:Bit),
        (_:Bit) ^ (_:Bit)
      )
    } yield op(left, right)

    def bitGen(level: Int): Gen[Bit] = for {
      bit <- Gen.lzy(
        Gen.frequency(
          ((5 - level).max(0), primitiveGen),
          ((10 - level).max(0), varGen),
          (level, notGen(level)),
          (level, binGen(level))
        )
      )
    } yield bit

    bitGen(10)
  }

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
