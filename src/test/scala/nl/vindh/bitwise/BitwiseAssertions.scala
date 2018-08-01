package nl.vindh.bitwise

import org.scalatest._

// TODO
import nl.vindh.bitwise.types.{ONE, ZERO}

trait BitwiseAssertions extends Matchers {
  def assertEquivalence(left: Bit, right: Bit, varnames: List[BitVar]): Unit = {
    require(varnames.length <= 16, "Too many variables for testing")

    (0 until scala.math.pow(2, varnames.length).toInt).foreach {
      testcase => {
        val valuation = (0 until varnames.length map {
          position => varnames(position) -> (if ((testcase & (1 << position)) != 0) ONE else ZERO)
        }).toMap

        assert(left.substitute(valuation) === right.substitute(valuation), valuation)
      }
    }
  }

  def assertTree(bit: Bit)(test: Bit => Boolean): Unit = {
    assert(test(bit), bit)
    bit match {
      case bin: BinaryOperator => {
        assertTree(bin.left)(test)
        assertTree(bin.right)(test)
      }
      case BitNot(not) => assertTree(not)(test)
      case BitVar(_) =>
      case _ => throw new Exception(s"Type not supported: ${bit.getClass.toString}")
    }
  }
}
