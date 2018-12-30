package nl.vindh.bitwise

import org.scalatest._

class MetricsTest extends FlatSpec with Matchers with BitVarXs {
  "Metrics.countOperators" should "count operators" in {
    // Arrange
    val f1 = x1 | (x3 ^ x2)
    val f2 = x1 ^ x2 ^ x3 ^ x4
    val f3 = x1 <-> x3
    val f4 = x1 ^ x2 ^ (x3 | x4 | (x5 ^ x6))
    val f5 = ONE // NOTE: we need to test this one seperately because in a formula it will be simplified away

    // Act
    val c1 = Metrics.countOperators(f1)
    val c2 = Metrics.countOperators(f2)
    val c3 = Metrics.countOperators(f3)
    val c4 = Metrics.countOperators(f4)
    val c5 = Metrics.countOperators(f5)

    // Assert
    // NOTE: these assertions may change whenever new simplifications are implemented
    assert(c1 === Metrics.OperatorCount(ops = 2, vars = 3, vals = 0, opTable = Map("^" -> 1, "|" -> 1)))
    assert(c2 === Metrics.OperatorCount(ops = 3, vars = 4, vals = 0, opTable = Map("^" -> 3)))
    assert(c3 === Metrics.OperatorCount(ops = 1, vars = 2, vals = 0, opTable = Map("<->" -> 1)))
    assert(c4 === Metrics.OperatorCount(ops = 5, vars = 6, vals = 0, opTable = Map("^" -> 3, "|" -> 2)))
    assert(c5 === Metrics.OperatorCount(ops = 0, vars = 0, vals = 1, opTable = Map()))
  }
}
