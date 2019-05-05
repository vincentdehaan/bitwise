package nl.vindh.bitwise

import org.scalatest.{FlatSpec, Matchers}

class SimplifierSpec extends FlatSpec with Matchers with BitVarXs with BitwiseAssertions {
  // Arrange
  implicit val config = SimplifierConfig(1)
  implicit val vargen = new VariableGenerator("t")

  "Simplifier.substituteSubtrees" should "substitute subtrees preserving equivalence" in {
    // Arrange
    val f1 = (x1 ^ x2) | (x3 & (x1 ^ x2))

    // Act
    val (g1, d1) = Simplifier.substituteSubtrees(f1)

    // Assert
    assertEquivalence(g1.substitute(d1), f1)
  }

  it should "do a full substitution if minimumOperatorCount = 1 and substituteAll = true" in {
    // Arrange
    val configAll = SimplifierConfig(minimumOperatorCount = 1, substituteAll = true)
    val bs1 = new BitSequence(List(x1 & x2 & x3, x2 | x3 | x4))
    val bs2 = BitSequence.variable("x", 4) + BitSequence.variable("y", 4)

    // Act
    val bswd1 = Simplifier.substituteSubtrees(bs1)(vargen, configAll)
    val (bss2, d2) = Simplifier.substituteSubtrees(bs2)(vargen, configAll)

    // Assert
    // TODO: make this working
    //assert(Metrics.countOperators(bswd1).ops === 0)
    //assert(Metrics.countOperators(bss2).ops === 0)
  }

  "Simplifier.substituteAll" should "handle repeated full substitutions correctly" in {
    // Arrange
    val bsx = BitSequence.variable("x", 32)
    val bsy = BitSequence.variable("y", 32)
    val bsz = BitSequence.variable("z", 32)
    val bsw = BitSequence.variable("w", 32)
    val bsxy = bsx + bsy

    // Act
    val (sbsxy, dxy) = Simplifier.substituteAll(bsxy)
    val bsxyz = sbsxy + bsz
    val (sbsxyz, dxyz) = Simplifier.substituteAll(bsxyz)
    val bsxyzw = sbsxyz + bsw
    val (sbsxyzw, dxyzw) = Simplifier.substituteAll(bsxyzw)

    // Assert
    // TODO
    val vx = Valuation(178, 32, "x")
    val vy = Valuation(23, 32, "y")
    val vz = Valuation(24, 32, "z")
    val vw = Valuation(2222, 32, "w")
    val v = vx ++ vy ++ vz ++ vw
    val s = sbsxyzw.substitute(dxy ++ dxyz ++ dxyzw).substitute(v)
    assert(s.toInt === (178 + 23 + 24 + 2222))
  }

  it should "be undone by substitute" in {
    // Arrange
    val bs = new BitSequence(List(x1 & x2, x2 & x3, !x4))

    // Act
    val (bss, d) = Simplifier.substituteAll(bs)
    val bs2 = bss.substitute(d)

    // Assert
    assertEquivalence(bs2, bs)
  }

  it should "substitute all formulas for a single variable" in {
    // Arrange
    val f1 = (x1 ^ x2) | (x3 & (x1 ^ x2))
    val bs1 = new BitSequence(List(x1 & x2, x2 & x3, !x4))

    // Act
    val (g1, d1) = Simplifier.substituteAll(f1)
    val (bss1, dd1) = Simplifier.substituteAll(bs1)

    // Assert
    assert(g1.isInstanceOf[BitVar])
    foreachValuation(getVariables(f1)) {
      valuation => assert(f1.substitute(valuation) === g1.substitute(valuation ++ d1))
    }
    bss1.bits.foreach {
      bit => assert(bit.isInstanceOf[BitVar])
    }
    // TODO: assert equivalence of bs1 and bswd1
  }

  it should "not substitute values and single variables" in {
    // Arrange
    val bs = new BitSequence(List(x1, x2 & x3, ONE))

    // Act
    val (bss, d) = Simplifier.substituteAll(bs)

    // Assert
    assert(bss.bits(0) === x1)
    assert(bss.bits(2) === ONE)
    assert(d.size === 1)
  }

  "Simplifier.getRelevantDefs" should "return only the defs that are relevant to the given formula" in {
    // Arrange
    val t1 = BitVar("t1")
    val t2 = BitVar("t2")
    val t3 = BitVar("t3")
    val f = t1 & x3
    val d = Map(t1 -> (t2 & x1), t2 -> x1, t3 -> !x3)

    // Act
    val rd = Simplifier.getRelevantDefs(f, d)

    // Assert
    assert(rd === Map(t1 -> (t2 & x1), t2 -> x1))
  }

  /*"BitSequenceWithDefs" should "behave like a BitSequence" in {
    // Arrange
    val x = BitSequence.variable("x", 8)
    val y = BitSequence.variable("y", 8)
    val z = BitSequence.variable("z", 8)
    val n = BitSequence(27, 8)
    val sum = x + y
    val (simplified, defs) = Simplifier.substituteSubtrees(sum)

    // Act
    val sum2 = simplified + z
    val xor = simplified ^ n
    val and = simplified & n

    // Assert
    val vx = Valuation(22, 8, "x")
    val vy = Valuation(19, 8, "y")
    val vz = Valuation(103, 8, "z")
    val sum2value = sum2.substitute(defs).substitute(vx).substitute(vy).substitute(vz)
    val xorvalue = xor.substitute(defs).substitute(vx).substitute(vy).substitute(vz)
    val andvalue = and.substitute(defs).substitute(vx).substitute(vy).substitute(vz)
    assert(sum2value.toInt === 144)
    assert(xorvalue.toInt === 50)
    assert(andvalue.toInt === 9)
  }*/
}
