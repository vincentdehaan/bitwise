package nl.vindh.bitwise

import types._

trait Bit extends Any{
  def & (that: Bit): Bit = BitAnd(this, that)
  def | (that: Bit): Bit = BitOr(this, that)
  def ^ (that: Bit): Bit = BitXor(this, that)
  def unary_! : Bit = BitNot(this)
}

class BitValue(val value: Int) extends AnyVal with Bit {
  override def toString: String = value.toString
}

abstract class BitFormula extends Bit {}

object BitAnd {
  def apply(left: Bit, right: Bit): Bit = left match {
    case vleft: BitValue => if(vleft.value == 1) right else ZERO
    case fleft: BitFormula => right match {
      case vright: BitValue => if(vright.value == 1) fleft else ZERO
      case fright: BitFormula => new BitAnd(fleft, fright)
    }
  }
}

class BitAnd (val left: Bit, val right: Bit) extends BitFormula {

}

object BitOr {
  def apply(left: Bit, right: Bit): Bit = left match {
    case vleft: BitValue => if(vleft.value == 1) ONE else right
    case fleft: BitFormula => right match {
      case vright: BitValue => if(vright.value == 1) ONE else fleft
      case fright: BitFormula => new BitOr(fleft, fright)
    }
  }
}

class BitOr (val left: Bit, val right: Bit) extends BitFormula {

}

object BitXor {
  def apply(left: Bit, right: Bit): Bit = (left, right) match {
    case (vleft: BitValue, vright: BitValue) => if(vleft.value == vright.value) ZERO else ONE
    case _ => new BitXor(left, right)
  }
}

class BitXor (val left: Bit, val right: Bit) extends BitFormula {

}

object BitNot {
  def apply(bit: Bit): Bit = bit match {
    case v: BitValue => if(v.value == 1) ZERO else ONE
    case f: BitFormula => new BitNot(f)
  }
}

class BitNot (val bit: Bit) extends BitFormula {}