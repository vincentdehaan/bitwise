package nl.vindh.bitwise

import org.scalatest.{FlatSpec, Matchers}

class SimplifierSpec extends FlatSpec with Matchers with BitVarXs with BitwiseAssertions {
  "Simplifier.substituteSubtrees" should "substitute subtrees preserving equivalence" in {
    // Arrange
    implicit val config = SimplifierConfig(1)
    implicit val vargen = new VariableGenerator("y")
    val f1 = (x1 ^ x2) | (x3 & (x1 ^ x2))

    // Act
    val fwd1 = Simplifier.substituteSubtrees(f1)

    // Assert
    foreachValuation(getVariables(f1)){
      valuation => assert(f1.substitute(valuation) === fwd1.substitute(valuation))
    }

  }
}
