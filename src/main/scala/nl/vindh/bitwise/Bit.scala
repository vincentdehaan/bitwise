package nl.vindh.bitwise

import types._

trait Bit extends Any{
  def & (that: Bit): Bit = BitAnd(this, that)
  def | (that: Bit): Bit = BitOr(this, that)
  def ^ (that: Bit): Bit = BitXor(this, that)
  def <-> (that: Bit): Bit = BitEq(this, that)
  def unary_! : Bit = BitNot(this)
  def substitute(vars: Map[BitVar, Bit]): Bit
  def isCnf: Boolean = false // Returns true only if this is a cnf that associates to the right in both AND and OR
  private def isCnfClause: Boolean = false
  private[bitwise] def onlyAndOrNot: Bit
  private[bitwise] def pushNotInside: Bit
  private[bitwise] def pushOrInside: Bit
  private[bitwise] def associateRight: Bit


  // TODO: clean up repetitions in or and and clauses
  // TODO: toCNF
}

// TODO: is this still a value type now that it's a case class?
case class BitValue(value: Boolean) extends AnyVal with Bit with Atomic {
  override def toString: String = if(value) "1" else "0"
  def substitute(vars: Map[BitVar, Bit]): Bit = this

}

abstract class BitFormula extends Bit {}

trait BinaryOperator {
  val left: Bit
  val right: Bit
  private[bitwise] def op: (Bit, Bit) => Bit
  private[bitwise] def onlyAndOrNot: Bit = op(left.onlyAndOrNot, right.onlyAndOrNot)
  private[bitwise] def pushNotInside: Bit = op(left.pushNotInside, right.pushNotInside)
  private[bitwise] lazy val pushOrInside: Bit = op(left.pushOrInside, right.pushOrInside)
}

trait Atomic extends Any with Bit {
  private[bitwise] def onlyAndOrNot: Bit = this
  private[bitwise] def pushNotInside: Bit = this
  private[bitwise] def pushOrInside: Bit = this
  private[bitwise] def associateRight: Bit = this
}

object BitAnd {
  def apply(left: Bit, right: Bit): Bit = left match {
    case vleft: BitValue => if(vleft.value) right else ZERO
    case fleft: BitFormula => right match {
      case vright: BitValue => if(vright.value) fleft else ZERO
      case fright: BitFormula => if(fleft == fright) fleft else new BitAnd(fleft, fright)
    }
  }
  private[bitwise] def forceBitAnd(left: Bit, right: Bit): BitAnd = {
    val res = left & right
    res match {
      case and: BitAnd => and
      case _ => throw new Exception(s"Expression simplifies to other type: ${res.getClass}")
    }
  }
}

case class BitAnd (left: Bit, right: Bit) extends BitFormula with BinaryOperator {
  override def toString: String = s"($left&$right)"
  def substitute(vars: Map[BitVar, Bit]): Bit = left.substitute(vars) & right.substitute(vars)
  def op: (Bit, Bit) => Bit = _ & _
  private[bitwise] def associateRight: BitAnd = this match {
    case BitAnd(left: BitAnd, right: BitAnd) =>
      val aLeft = left.associateRight
      val aRight = right.associateRight // now aLeft.left and aRight.left are not BitAnd anymore
      BitAnd.forceBitAnd(aLeft.left, aRight.left & (aLeft.right & aRight.right).associateRight)
    case BitAnd(left: BitAnd, right: Bit) => BitAnd.forceBitAnd(right, left.associateRight)
    case _ => this
  }
}

object BitOr {
  def apply(left: Bit, right: Bit): Bit = left match {
    case vleft: BitValue => if(vleft == ONE) ONE else right
    case fleft: BitFormula => right match {
      case vright: BitValue => if(vright == ONE) ONE else fleft
      case fright: BitFormula => if(fleft == fright) fleft else new BitOr(fleft, fright)
    }
  }
  private[bitwise] def forceBitOr(left: Bit, right: Bit): BitOr = {
    val res = left | right
    res match {
      case or: BitOr => or
      case _ => throw new Exception(s"Expression simplifies to other type: ${res.getClass}")
    }
  }
}

case class BitOr (left: Bit, right: Bit) extends BitFormula with BinaryOperator {
  override def toString: String = s"($left|$right)"
  def substitute(vars: Map[BitVar, Bit]): Bit = left.substitute(vars) | right.substitute(vars)
  def op: (Bit, Bit) => Bit = _ | _
  override private[bitwise] lazy val pushOrInside: Bit = {
    val leftOrInside = left.pushOrInside
    val rightOrInside = right.pushOrInside
    leftOrInside match {
      case BitAnd(andLeft, andRight) => (andLeft | rightOrInside).pushOrInside & (andRight | rightOrInside).pushOrInside
      case _ => rightOrInside match {
        case BitAnd(andLeft, andRight) => (andLeft | leftOrInside).pushOrInside & (andRight | leftOrInside).pushOrInside
        case _ => leftOrInside | rightOrInside
      }
    }
  }
  private[bitwise] def associateRight: BitOr = this match {
    case BitOr(left:BitOr, right: BitOr) =>
      val aLeft = left.associateRight
      val aRight = right.associateRight
      BitOr.forceBitOr(aLeft.left, aRight.left | (aLeft.right | aRight.right).associateRight)
    case BitOr(left: BitOr, right: Bit) => BitOr.forceBitOr(right, left.associateRight)
    case _ => this
  }
}

object BitXor {
  def apply(left: Bit, right: Bit): Bit = (left, right) match {
    case (vleft: BitValue, vright: BitValue) => if(vleft.value == vright.value) ZERO else ONE
    case (vleft: BitValue, fright: Bit) => if(vleft == ONE) !fright else fright
    case (fleft: Bit, vright: BitValue) => if(vright == ONE) !fleft else fleft
    case _ => if(left == right) ZERO else new BitXor(left, right)
  }
}

case class BitXor (left: Bit, right: Bit) extends BitFormula with BinaryOperator {
  override def toString: String = s"($left^$right)"
  def substitute(vars: Map[BitVar, Bit]): Bit = left.substitute(vars) ^ right.substitute(vars)
  def op: (Bit, Bit) => Bit = _ ^ _
  override private[bitwise] def onlyAndOrNot: Bit = {
    val leftAndOrNot = left.onlyAndOrNot
    val rightAndOrNot = right.onlyAndOrNot
    (leftAndOrNot & !rightAndOrNot) | (!leftAndOrNot & rightAndOrNot)
  }
  override private[bitwise] def pushNotInside: Bit = throw new Exception("pushNotInside should only be called after onlyAndOrNot")
  // TODO: override lazy val
  //override private[bitwise] def pushOrInside: Bit = throw new Exception("pushOrInside should only be called after onlyAndOrNot and pushNotInside")
  private[bitwise] def associateRight: Bit = ???
}

object BitEq {
  def apply(left: Bit, right: Bit): Bit = (left, right) match {
    case (vleft: BitValue, vright: BitValue) => if(vleft.value == vright.value) ONE else ZERO
    case (vleft: BitValue, fright: Bit) => if (vleft == ONE) fright else !fright
    case (fleft: Bit, vright: BitValue) => if (vright == ONE) fleft else !fleft
    case _ => if(left == right) ONE else new BitEq(left, right)
  }
}

case class BitEq (left: Bit, right: Bit) extends BitFormula with BinaryOperator {
  override def toString: String = s"($left<->$right)"
  def substitute(vars: Map[BitVar, Bit]): Bit = left.substitute(vars) <-> right.substitute(vars)
  def op: (Bit, Bit) => Bit = _ <-> _
  override private[bitwise] def onlyAndOrNot: Bit = {
    val leftAndOrNot = left.onlyAndOrNot
    val rightAndOrNot = right.onlyAndOrNot
    (!leftAndOrNot | rightAndOrNot) & (leftAndOrNot | !rightAndOrNot)
  }
  override private[bitwise] def pushNotInside: Bit = throw new Exception("pushNotInside should only be called after onlyAndOrNot")
  //override private[bitwise] def pushOrInside: Bit = throw new Exception("pushOrInside should only be called after onlyAndOrNot and pushNotInside")
  private[bitwise] def associateRight: Bit = ???
}

object BitNot {
  def apply(bit: Bit): Bit = bit match {
    case BitValue(v) => if(v) ZERO else ONE
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
  private[bitwise] def pushOrInside: Bit = !bit.pushOrInside
  private[bitwise] def associateRight: Bit = !bit.associateRight
}

object BitVar {
  def apply(name: String): BitVar = new BitVar(name)
}

case class BitVar (name: String) extends BitFormula with Atomic {
  override def toString: String = name
  def substitute(vars: Map[BitVar, Bit]): Bit = vars.getOrElse(this, this)
}