package nl.vindh.bitwise

import org.scalatest._

trait BitwiseAssertions extends Matchers {
  // TODO: enhance assert error messages
  def getVariables(bit: Bit): Set[BitVar] = bit match {
    case bvar: BitVar => Set(bvar)
    case bin: BinaryOperator => getVariables(bin.left) ++ getVariables(bin.right) // TODO: remove
    case ass: AssociativeOperator => ass.bits.flatMap(bit => getVariables(bit)).toSet
    case BitNot(not) => getVariables(not)
    case _ => Set()
  }

  def foreachValuation(vars: Set[BitVar])(f: Map[BitVar, Bit] => Unit): Unit = {
    val varList = vars.toList
    (0 until scala.math.pow(2, varList.size).toInt).foreach {
      i => {
        val valuation = (0 until varList.size map {
          position => varList(position) -> (if ((i & (1 << position)) != 0) ONE else ZERO)
        }).toMap
        f(valuation)
      }
    }
  }

  def findValuation(vars: Set[BitVar])(f: Map[BitVar, Bit] => Boolean): Option[Map[BitVar, Bit]] = {
    val varList = vars.toList
    (0 until scala.math.pow(2, varList.size).toInt).find {
      i => {
        val valuation = (0 until varList.size map {
          position => varList(position) -> (if ((i & (1 << position)) != 0) ONE else ZERO)
        }).toMap
        f(valuation)
      }
    }.map{
      i => { // TODO: code reuse
        (0 until varList.size map {
          position => varList(position) -> (if ((i & (1 << position)) != 0) ONE else ZERO)
        }).toMap
      }
    }
  }

  def assertEquivalence(left: Bit, right: Bit): Unit = {
    val varnamesLeft = getVariables(left)

    foreachValuation(varnamesLeft){
      valuation => {
        assert(left.substitute(valuation) === right.substitute(valuation), valuation)
        assert(left.substitute(valuation).isInstanceOf[BitValue])
        assert(right.substitute(valuation).isInstanceOf[BitValue])
      }
    }
  }

  def assertEquivalence(left: BitSequence, right: BitSequence): Unit = {
    assert(left.bits.size === right.bits.size)
    left.bits.zip(right.bits).foreach {
      case (l, r) => assertEquivalence(l, r)
    }
  }

  // Asserts that f === op(left, right)
  def assertBinaryOperator(f: Bit, left: Bit, right: Bit, op: (Boolean, Boolean) => Boolean): Unit = {
    def bitToBool(b: Bit): Boolean = if(b == ONE) true else false
    val varnames = getVariables(left) ++ getVariables(right)
    foreachValuation(varnames){
      valuation => {
        val lbool = bitToBool(left.substitute(valuation))
        val rbool = bitToBool(right.substitute(valuation))
        val fbool = bitToBool(f.substitute(valuation))

        assert(fbool === op(lbool, rbool))
      }
    }
  }

  /*
   * Test for two criteria:
   *  I.  For all valuations a of the vars of orig: if a satisfies orig, then there is a valuation t of the Tseitin vars
   *      such a combined with t satisfies tseitin.
   *  II. For all valuations of tseitin that satisfy tseitin, they also satisfy orig.
   */
  def assertTseitinEquivalence(orig: Bit, tseitin: Bit): Unit = {
    val varNamesOrig = getVariables(orig)
    val varNames = getVariables(tseitin)
    val varNamesNew = varNames -- varNamesOrig

    // Test I.
    foreachValuation(varNamesOrig){
      valuation => if(orig.substitute(valuation) == ONE){
        val valOpt = findValuation(varNamesNew){
          newValuation => tseitin.substitute(valuation ++ newValuation) == ONE
        }
        assert(valOpt.size === 1)
      }
    }

    // Test II.
    foreachValuation(varNames){
      valuation => if(tseitin.substitute(valuation) == ONE){
        assert(orig.substitute(valuation) == ONE, (orig, tseitin, valuation))
      }
    }
  }

  /*
   * Asserts that test(leaf) holds for every leaf in the tree
   */
  def assertTree(bit: Bit)(test: Bit => Boolean): Unit = {
    assert(test(bit), bit)
    bit match {
      case bin: BinaryOperator => { // TODO: remove
        assertTree(bin.left)(test)
        assertTree(bin.right)(test)
      }
      case ass: AssociativeOperator => ass.bits.foreach(assertTree(_)(test))
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
      case BitAnd(_ :: _) | BitOr(_ :: _) | BitNot(_) | BitVar(_) | BitValue(_) => true
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
      case or: BitOr[_] => or.bits.forall{
        _ match {
          case _: BitAnd[_] => false
          case _ => true
        }
      }
      case _ => true
    }
  }

  def assertCnf(bit: Bit): Unit = bit match {
    case BitAnd(lst) => lst.foreach {
      case BitOr(lst) => lst.foreach {
        case BitValue(_) | BitVar(_) | BitNot(BitVar(_)) =>
        case x => throw new Exception(s"No CNF! Expected: value, var or Not, found: $x")
      }
      case BitVar(_) | BitValue(_) =>
      case x => throw new Exception(s"No CNF! Expected: Or, found: $x")
    }
    case BitVar(_) | BitValue(_) =>
    case x => throw new Exception(s"No CNF! Expected: And, found: $x")
  }
}
