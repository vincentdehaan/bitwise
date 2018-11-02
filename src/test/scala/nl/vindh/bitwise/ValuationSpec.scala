package nl.vindh.bitwise

import org.scalatest.{FlatSpec, Matchers}

class ValuationSpec extends FlatSpec with Matchers with BitVarXs {
  "Valuation.apply" should "return a Valuation based on an integer" in {
    // Act
    val v = Valuation(7, 4, "x")

    // Assert
    assert(v === Map(x0 -> ONE, x1 -> ONE, x2 -> ONE, x3 -> ZERO))
  }
}
