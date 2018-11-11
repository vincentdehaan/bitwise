package nl.vindh.bitwise

import org.scalatest._

class TseitinSpec extends FlatSpec with Matchers with BitwiseAssertions with BitVarXs {
  "Tseitin.transform" should "generate a CNF that is equisatisfiable" in {
    // Arrange
    val f1 = ((x1 | x2) & x3) <-> !x4
    val f2 = !x5 | (x1 & x2 | x3 <-> x4)
    val f3 = x1
    val f4 = x1 & x2
    val f5 = !x1 & x2
    val f6 = (x3 ^ x5 ^ x7 ^ ((x2 ^ x4) & x6))
    val f7 = x7 ^ ((x2 ^ x4) & x6)
    val f8 = x5 ^ x7 ^ ((x2 ^ x4) & x6)
    val f9 = !(x0&x2)
    val f10 = x1 ^ x2
    val f11 = (x3 ^ x5 ^ x7 ^ (x2 & x6))
    val f12 = (x3 ^ (x2 & x6))

    // Act
    val t1 = Tseitin.transform(f1)
    val t2 = Tseitin.transform(f2)
    val t3 = Tseitin.transform(f3)
    val t4 = Tseitin.transform(f4)
    val t5 = Tseitin.transform(f5)
    val t6 = Tseitin.transform(f6)
    val t7 = Tseitin.transform(f7)
    val t8 = Tseitin.transform(f8)
    val t9 = Tseitin.transform(f9)
    val t10 = Tseitin.transform(f10)
    val t11 = Tseitin.transform(f11)
    val t12 = Tseitin.transform(f12)

    // Assert
    assertTseitinEquivalence(f1, t1)
    assertCnf(t1)
    assertTseitinEquivalence(f2, t2)
    assertCnf(t2)
    assertTseitinEquivalence(f3, t3)
    assertCnf(t3)
    assertTseitinEquivalence(f4, t4)
    assertCnf(t4)
    assertTseitinEquivalence(f5, t5)
    assertCnf(t5)
    assertTseitinEquivalence(f6, t6)
    assertCnf(t6)
    assertTseitinEquivalence(f7, t7)
    assertCnf(t7)
    assertTseitinEquivalence(f8, t8)
    assertCnf(t8)
    assertTseitinEquivalence(f9, t9)
    assertCnf(t9)
    assertTseitinEquivalence(f10, t10)
    assertCnf(t10)
    assertTseitinEquivalence(f11, t11)
    assertCnf(t11)
    assertTseitinEquivalence(f12, t12)
    assertCnf(t12)
  }

  "Tseitin.transform" should "be able to handle BitSequences as well" in {
    // TODO
  }
}