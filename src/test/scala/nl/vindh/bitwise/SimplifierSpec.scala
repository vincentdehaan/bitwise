package nl.vindh.bitwise

import org.scalatest.{FlatSpec, Matchers}

class SimplifierSpec extends FlatSpec with Matchers with BitVarXs with BitwiseAssertions {
  // Arrange
  implicit val config = SimplifierConfig(1)
  implicit val vargen = new VariableGenerator("test")

  "Simplifier.substituteSubtrees" should "substitute subtrees preserving equivalence" in {
    // Arrange
    val f1 = (x1 ^ x2) | (x3 & (x1 ^ x2))

    // Act
    val fwd1 = Simplifier.substituteSubtrees(f1)

    // Assert
    foreachValuation(getVariables(f1)){
      valuation => assert(f1.substitute(valuation) === fwd1.substitute(valuation))
    }

  }

  "BitSequenceWithDefs" should "behave like a BitSequence" in {
    // Arrange
    val x = BitSequence.variable("x", 8)
    val y = BitSequence.variable("y", 8)
    val z = BitSequence.variable("z", 8)
    val n = BitSequence(27, 8)
    val sum = x + y
    val simplified = Simplifier.substituteSubtrees(sum)

    // Act
    val sum2 = simplified + z
    val xor = simplified ^ n
    val and = simplified & n

    // Assert
    val vx = Valuation(22, 8, "x")
    val vy = Valuation(19, 8, "y")
    val vz = Valuation(103, 8, "z")
    val sum2value = sum2.substitute(vx).substitute(vy).substitute(vz)
    val xorvalue = xor.substitute(vx).substitute(vy).substitute(vz)
    val andvalue = and.substitute(vx).substitute(vy).substitute(vz)
    assert(sum2value.toInt === 144)
    assert(xorvalue.toInt === 50)
    assert(andvalue.toInt === 9)
  }
}
