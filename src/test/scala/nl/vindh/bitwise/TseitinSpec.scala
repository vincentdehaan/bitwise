package nl.vindh.bitwise

import org.scalatest._
import types._

class TseitinSpec extends FlatSpec with Matchers with BitwiseAssertions {
  // Arrange
  val x1 = BitVar("x1")
  val x2 = BitVar("x2")
  val x3 = BitVar("x3")
  val x4 = BitVar("x4")
  val x5 = BitVar("x5")

  "Tseitin.transform" should "generate a CNF that is equisatisfiable" in {
    // Arrange
    val f1 = ((x1 | x2) & x3) <-> !x4
    val f2 = !x5 | (x1 & x2 | x3 <-> x4)
    val f3 = x1
    val f4 = x1 & x2
    val f5 = !x1 & x2

    // Act
    val t1 = Tseitin.transform(f1)
    val t2 = Tseitin.transform(f2)
    val t3 = Tseitin.transform(f3)
    val t4 = Tseitin.transform(f4)
    val t5 = Tseitin.transform(f5)

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

  }
}