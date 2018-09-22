package nl.vindh.bitwise

import org.scalatest._

import nl.vindh.bitwise.types.{ONE, ZERO}

trait BitwiseAssertions extends Matchers {
  // TODO: enhance assert error messages
  def assertEquivalence(left: Bit, right: Bit): Unit = {
    def getVariables(bit: Bit): Set[BitVar] = bit match {
      case bvar: BitVar => Set(bvar)
      case bin: BinaryOperator => getVariables(bin.left) ++ getVariables(bin.right)
      case BitNot(not) => getVariables(not)
      case _ => Set()
    }

    val varnamesLeft = getVariables(left)
    val varnamesRight = getVariables(right)
    if(varnamesLeft != varnamesRight) throw new Exception("Formulas don't have the same variables!")

    val varnames = varnamesLeft.toList

    (0 until scala.math.pow(2, varnames.size).toInt).foreach {
      testcase => {
        val valuation = (0 until varnames.size map {
          position => varnames(position) -> (if ((testcase & (1 << position)) != 0) ONE else ZERO)
        }).toMap
        assert(left.substitute(valuation) === right.substitute(valuation), valuation)
      }
    }
  }

  /*
   * Asserts that test(leaf) holds for every leaf in the tree
   */
  def assertTree(bit: Bit)(test: Bit => Boolean): Unit = {
    assert(test(bit), bit)
    bit match {
      case bin: BinaryOperator => {
        assertTree(bin.left)(test)
        assertTree(bin.right)(test)
      }
      case BitNot(not) => assertTree(not)(test)
      case BitVar(_) | BitValue(_) =>
      case _ => throw new Exception(s"Type not supported: ${bit.getClass.toString}")
    }
  }

  /*
   * Asserts that this formula only contains &, | and !
   */
  def assertAndOrNot(bit: Bit): Unit = assertTree(bit){
    _ match {
      case BitAnd(_, _) | BitOr(_, _) | BitNot(_) | BitVar(_) | BitValue(_) => true
      case _ => false
    }
  }

  /*
   * Asserts that in this formula ! is only applied to atomic subformulas (i.e. variables)
   */
  def assertNotInside(bit: Bit): Unit = assertTree(bit){
    _ match {
      case BitNot(_: Atomic) => true
      case BitNot(_) => false
      case _ => true
    }
  }

  /*
   * Asserts that in this formula | is never applied to & clauses
   */
  def assertOrInside(bit: Bit): Unit = assertTree(bit){
    _ match {
      case BitOr(_: BitAnd, _) => false
      case BitOr(_, _: BitAnd) => false
      case _ => true
    }
  }

  /*
   * Asserts that no & has another & as left argument, and no | has another | as left argument
   */
  def assertAssociateRight(bit: Bit): Unit = assertTree(bit){
    _ match {
      case BitOr(left: BitOr, _) => false
      case BitAnd(left: BitAnd, _) => false
      case _ => true
    }
  }
}
