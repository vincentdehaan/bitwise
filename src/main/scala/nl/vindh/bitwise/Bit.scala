package nl.vindh.bitwise

trait Bit extends Any{
  def & (that: Bit): Bit = BitAnd(List(this, that))
  def | (that: Bit): Bit = BitOr(List(this, that))
  def ^ (that: Bit): Bit = BitXor(List(this, that))
  def <-> (that: Bit): Bit = BitEq(this, that)
  def unary_! : Bit = BitNot(this)
  def substitute(defs: Defs): Bit
  def isCnf: Boolean = false // Returns true only if this is a cnf that associates to the right in both AND and OR
  private def isCnfClause: Boolean = false
  private[bitwise] def onlyAndOrNot: Bit
  private[bitwise] def pushNotInside: Bit
  private[bitwise] def pushOrInside: Bit

  // TODO: clean up repetitions in or and and clauses
}

// TODO: is this still a value type now that it's a case class?
case class BitValue(value: Boolean) extends AnyVal with Bit with Atomic {
  override def toString: String = if(value) "1" else "0"
  def substitute(vars: Map[BitVar, Bit]): Bit = this

}

abstract class BitFormula extends Bit {}

trait BinaryOperator extends Bit {
  val left: Bit
  val right: Bit
  private[bitwise] val opSymbol: String
  private[bitwise] def op: (Bit, Bit) => Bit
  private[bitwise] def onlyAndOrNot: Bit = op(left.onlyAndOrNot, right.onlyAndOrNot)
  private[bitwise] def pushNotInside: Bit = op(left.pushNotInside, right.pushNotInside)
  private[bitwise] def pushOrInside: Bit = op(left.pushOrInside, right.pushOrInside)
}

trait Atomic extends Any with Bit {
  private[bitwise] def onlyAndOrNot: Bit = this
  private[bitwise] def pushNotInside: Bit = this
  private[bitwise] def pushOrInside: Bit = this
}

trait AssociativeOperator extends Bit {
  val bits: Iterable[Bit]
  override def toString: String = bits.mkString("(", opSymbol, ")")
  private[bitwise] val opSymbol: String
  private[bitwise] def op: (Bit, Bit) => Bit
  private[bitwise] lazy val onlyAndOrNot: Bit = bits.tail.fold(bits.head)((l, r) => op(l.onlyAndOrNot, r.onlyAndOrNot))
  private[bitwise] lazy val pushNotInside: Bit = bits.tail.fold(bits.head)((l, r) => op(l.pushNotInside, r.pushNotInside))
  private[bitwise] lazy val pushOrInside: Bit = bits.tail.fold(bits.head)((l, r) => op(l.pushOrInside, r.pushOrInside))
}

object BitAnd {
  // TODO: handle !x1 & x1
  private def cleanList(clean: List[Bit], bits: List[Bit]): List[Bit] =
    bits match {
      case Nil => clean
      case ZERO :: _ => List(ZERO)
      case ONE :: Nil => clean
      case ONE :: tl => cleanList(clean, tl)
      case BitNot(n) :: _ if clean.contains(n) => List(ZERO)
      case v :: tl if clean.contains(v) => cleanList(clean, tl)
      // TODO: the next two clauses are not tail-recursive, is this a problem?
      case (a: BitAnd[_]) :: tl => cleanList(clean ::: cleanList(Nil, a.bits), tl)
      case v :: tl => cleanList(v :: clean, tl)
    }

  def apply[T <: Bit](bits: List[T]): Bit = // TODO: optimize this in one traversal
    cleanList(Nil, bits) match {
      case Nil => ONE
      case v :: Nil => v
      case lst => new BitAnd(lst)
    }
}

case class BitAnd[T <: Bit] (bits: List[T]) extends BitFormula with AssociativeOperator {
  private[bitwise] val opSymbol = "&"
  def substitute(vars: Defs): Bit = BitAnd(bits.map(_.substitute(vars)))
  def op: (Bit, Bit) => Bit = _ & _
}

object BitOr {
  // TODO: handle !x1 | x1
  private def cleanList(clean: List[Bit], bits: List[Bit]): List[Bit] =
    bits match {
      case Nil => clean
      case ONE :: _ => List(ONE)
      case ZERO :: Nil => clean
      case ZERO :: tl => cleanList(clean, tl)
      case BitNot(n) :: tl if clean.contains(n) => List(ONE)
      case v :: tl if clean.contains(v) => cleanList(clean, tl)
      // TODO: the next two clauses are not tail-recursive, is this a problem?
      case (o: BitOr[_]) :: tl => cleanList(clean ::: cleanList(Nil, o.bits), tl)
      case v :: tl => cleanList(v :: clean, tl)
    }

  def apply[T <: Bit] (bits: List[T]): Bit =
    cleanList(Nil, bits) match {
      case Nil => ZERO
      case v :: Nil => v
      case lst => new BitOr(lst)
    }
}

case class BitOr[T <: Bit] (bits: List[T]) extends BitFormula with AssociativeOperator {
  private[bitwise] val opSymbol = "|"
  def substitute(vars: Defs): Bit = BitOr(bits.map(_.substitute(vars)))
  def op: (Bit, Bit) => Bit = _ | _
  override private[bitwise] lazy val pushOrInside: Bit = {
    val headOrInside = bits.head.pushOrInside
    val tailOrInside = BitOr(bits.tail).pushOrInside
    headOrInside match {
      case BitAnd(hd :: tl) => (hd | tailOrInside).pushOrInside & (BitAnd(tl) | tailOrInside).pushOrInside
      case _ => tailOrInside match {
        case BitAnd(hd :: tl) => (headOrInside | hd).pushOrInside & (headOrInside | BitAnd(tl)).pushOrInside
        case _ => headOrInside | tailOrInside
      }
    }
  }
}

object BitXor {
  def apply[T <: Bit] (bits: List[T]): Bit = {
    val countOnes = bits.count(_ == ONE)
    val cleanBits = bits.flatMap {
      case xor: BitXor[_] => xor.bits
      case _: BitValue => Nil
      case bit => List(bit)
    }

    val cleanBitsWithOne = if(countOnes % 2 == 1) ONE :: cleanBits else cleanBits
    cleanBitsWithOne match {
      case Nil => ZERO
      case hd :: Nil => hd
      case ONE :: x :: Nil => BitNot(x)
      case lst => new BitXor(lst)
    }
  }
}

case class BitXor[T <: Bit] (bits: List[T]) extends BitFormula with AssociativeOperator {
  private[bitwise] val opSymbol = "^"
  def substitute(vars: Defs): Bit = BitXor(bits.map(_.substitute(vars)))
  def op: (Bit, Bit) => Bit = _ ^ _
  override private[bitwise] lazy val onlyAndOrNot: Bit = {
    val headAndOrNot = bits.head.onlyAndOrNot
    val tailAndOrNot = BitXor(bits.tail).onlyAndOrNot
    (headAndOrNot & !tailAndOrNot) | (!headAndOrNot & tailAndOrNot)
  }
  override private[bitwise] lazy val pushNotInside: Bit = throw new Exception("pushNotInside should only be called after onlyAndOrNot")
  override private[bitwise] lazy val pushOrInside: Bit = throw new Exception("pushOrInside should only be called after onlyAndOrNot and pushNotInside")

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
  override val opSymbol: String = "<->"
  def substitute(vars: Defs): Bit = left.substitute(vars) <-> right.substitute(vars)
  def op: (Bit, Bit) => Bit = _ <-> _
  override private[bitwise] def onlyAndOrNot: Bit = {
    val leftAndOrNot = left.onlyAndOrNot
    val rightAndOrNot = right.onlyAndOrNot
    (!leftAndOrNot | rightAndOrNot) & (leftAndOrNot | !rightAndOrNot)
  }
}

object BitNot {
  def apply[T <: Bit](bit: T): Bit = bit match {
    case BitValue(v) => if (v) ZERO else ONE
    case BitNot(n) => n
    case f: BitFormula => new BitNot(f)
  }
}

case class BitNot[T <: Bit] (bit: T) extends BitFormula {
  override def toString: String = s"(!$bit)"
  def substitute(vars: Defs): Bit = !bit.substitute(vars)
  private[bitwise] def onlyAndOrNot: Bit = !bit.onlyAndOrNot
  private[bitwise] def pushNotInside: Bit = bit match { // Apply De Morgan's laws
    case BitAnd(bits) => BitOr(bits.map(! _)).pushNotInside
    case BitOr(bits) => BitAnd(bits.map(! _)).pushNotInside
    case BitVar(_) => this
    case _ => throw new Exception(s"pushNotInside should only be called after onlyAndOrNot. Type found: ${bit.getClass}")
  }
  private[bitwise] def pushOrInside: Bit = !bit.pushOrInside
}

object BitVar {
  def apply(name: String): BitVar = new BitVar(name)
}
// TODO: add integer-based BitVar
case class BitVar (name: String) extends BitFormula with Atomic {
  override def toString: String = name
  def substitute(vars: Defs): Bit = vars.getOrElse(this, this) match {
    case v: BitValue => v
    case v: BitVar if !vars.contains(v) => v
    case f => f.substitute(vars)
  }

}