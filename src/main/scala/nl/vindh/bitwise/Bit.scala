package nl.vindh.bitwise

import types._

trait Bit extends Any{
  def & (that: Bit): Bit = BitAnd(this, that)
  def | (that: Bit): Bit = BitOr(this, that)
  def ^ (that: Bit): Bit = BitXor(this, that)
  def <-> (that: Bit): Bit = BitEq(this, that)
  def unary_! : Bit = BitNot(this)
  def substitute(vars: Map[BitVar, Bit]): Bit
  private[bitwise] def onlyAndOrNot: Bit // TODO: write tests
  private[bitwise] def pushNotInside: Bit // TODO: write tests
  // TODO: pushOrInside
  // TODO: toCNF
}

// TODO: is this still a value type now that it's a case class?
case class BitValue(value: Int) extends AnyVal with Bit with Atomic {
  override def toString: String = value.toString
  def substitute(vars: Map[BitVar, Bit]): Bit = this
  private[bitwise] def onlyAndOrNot: Bit = this
  private[bitwise] def pushNotInside: Bit = this
}

abstract class BitFormula extends Bit {}

trait BinaryOperator {
  val left: Bit
  val right: Bit
}

trait Atomic extends Any with Bit

object BitAnd {
  def apply(left: Bit, right: Bit): Bit = left match {
    case vleft: BitValue => if(vleft.value == 1) right else ZERO
    case fleft: BitFormula => right match {
      case vright: BitValue => if(vright.value == 1) fleft else ZERO
      case fright: BitFormula => if(fleft == fright) fleft else new BitAnd(fleft, fright)
    }
  }
}

case class BitAnd (left: Bit, right: Bit) extends BitFormula with BinaryOperator {
  override def toString: String = s"($left&$right)"
  def substitute(vars: Map[BitVar, Bit]): Bit = left.substitute(vars) & right.substitute(vars)
  private[bitwise] def onlyAndOrNot: Bit = left.onlyAndOrNot & right.onlyAndOrNot
  private[bitwise] def pushNotInside: Bit = left.pushNotInside & right.pushNotInside
}

object BitOr {
  def apply(left: Bit, right: Bit): Bit = left match {
    case vleft: BitValue => if(vleft.value == 1) ONE else right
    case fleft: BitFormula => right match {
      case vright: BitValue => if(vright.value == 1) ONE else fleft
      case fright: BitFormula => if(fleft == fright) fleft else new BitOr(fleft, fright)
    }
  }
}

case class BitOr (left: Bit, right: Bit) extends BitFormula with BinaryOperator {
  override def toString: String = s"($left|$right)"
  def substitute(vars: Map[BitVar, Bit]): Bit = left.substitute(vars) | right.substitute(vars)
  private[bitwise] def onlyAndOrNot: Bit = left.onlyAndOrNot | right.onlyAndOrNot
  private[bitwise] def pushNotInside: Bit = left.pushNotInside | right.pushNotInside
}

object BitXor {
  def apply(left: Bit, right: Bit): Bit = (left, right) match {
    case (vleft: BitValue, vright: BitValue) => if(vleft.value == vright.value) ZERO else ONE
    case _ => if(left == right) ZERO else new BitXor(left, right)
  }
}

case class BitXor (left: Bit, right: Bit) extends BitFormula with BinaryOperator {
  override def toString: String = s"($left^$right)"
  def substitute(vars: Map[BitVar, Bit]): Bit = left.substitute(vars) ^ right.substitute(vars)
  private[bitwise] def onlyAndOrNot: Bit = {
    val leftAndOrNot = left.onlyAndOrNot
    val rightAndOrNot = right.onlyAndOrNot
    (leftAndOrNot & !rightAndOrNot) | (!leftAndOrNot & rightAndOrNot)
  }
  private[bitwise] def pushNotInside: Bit = throw new Exception("pushNotInside should only be called after onlyAndOrNot")
}

object BitEq {
  def apply(left: Bit, right: Bit): Bit = (left, right) match {
    case (vleft: BitValue, vright: BitValue) => if(vleft.value == vright.value) ONE else ZERO
    case _ => if(left == right) ONE else new BitEq(left, right)
  }
}

case class BitEq (left: Bit, right: Bit) extends BitFormula with BinaryOperator {
  override def toString: String = s"($left==$right)"
  def substitute(vars: Map[BitVar, Bit]): Bit = left.substitute(vars) <-> right.substitute(vars)
  private[bitwise] def onlyAndOrNot: Bit = {
    val leftAndOrNot = left.onlyAndOrNot
    val rightAndOrNot = right.onlyAndOrNot
    (leftAndOrNot & rightAndOrNot) | (!leftAndOrNot & !rightAndOrNot)
  }
  private[bitwise] def pushNotInside: Bit = throw new Exception("pushNotInside should only be called after onlyAndOrNot")
}

object BitNot {
  def apply(bit: Bit): Bit = bit match {
    case BitValue(v) => if(v == 1) ZERO else ONE
    case BitNot(n) => n
    case f: BitFormula => new BitNot(f)
  }
}

case class BitNot (bit: Bit) extends BitFormula {
  override def toString: String = s"(!$bit)"
  def substitute(vars: Map[BitVar, Bit]): Bit = !bit.substitute(vars)
  private[bitwise] def onlyAndOrNot: Bit = !bit.onlyAndOrNot
  private[bitwise] def pushNotInside: Bit = bit match { // Apply De Morgan's laws
    case BitAnd(left, right) => (!left | !right).pushNotInside
    case BitOr(left, right) => (!left & !right).pushNotInside
    case BitVar(_) => this
    case _ => throw new Exception(s"pushNotInside should only be called after onlyAndOrNot. Type found: ${bit.getClass}")
  }
}

object BitVar {
  def apply(name: String): BitVar = new BitVar(name)
}

case class BitVar (name: String) extends BitFormula with Atomic {
  override def toString: String = name
  def substitute(vars: Map[BitVar, Bit]): Bit = vars.getOrElse(this, this)
  private[bitwise] def onlyAndOrNot: Bit = this
  private[bitwise] def pushNotInside: Bit = this
}