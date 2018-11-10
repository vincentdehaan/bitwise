package nl.vindh.bitwise

trait Bit extends Any{
  def & (that: Bit): Bit = BitAnd(List(this, that))
  def | (that: Bit): Bit = BitOr(List(this, that))
  def ^ (that: Bit): Bit = BitXor(List(this, that))
  def <-> (that: Bit): Bit = BitEq(this, that)
  def unary_! : Bit = BitNot(this)
  def substitute(vars: Map[BitVar, Bit]): Bit
  def isCnf: Boolean = false // Returns true only if this is a cnf that associates to the right in both AND and OR
  private def isCnfClause: Boolean = false
  private[bitwise] def onlyAndOrNot: Bit
  private[bitwise] def pushNotInside: Bit
  private[bitwise] def pushOrInside: Bit

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
  def apply[T <: Bit](bits: List[T]): Bit = // TODO: optimize this in one traversal
    if(bits.exists(_ == ZERO)) ZERO
    else if(bits.forall(_ == ONE)) ONE
    else {
      val cleanBits = bits.flatMap(
        bit => bit match {
          case and: BitAnd[_] => and.bits // TODO: is pattern matching on BitAnd[T] impossible due to type erasure?
          case ONE => Nil // We already know that not ALL elements are ONE
          case bit => List(bit)
        }
      ).foldRight((Nil: List[Bit], Set(): Set[Bit], Set(): Set[Bit], ONE)) { // TODO: break loop earlier
        (l, r) => // TODO: _1 and _2 are the same; remove _1; also at BitOr
          l match {
            case BitNot(b) if(r._2.contains(b)) => (r._1, r._2, r._3, ZERO)
            case b if(r._3.contains(b)) => (r._1, r._2, r._3, ZERO)
            case b if(r._2.contains(b)) => r
            case b => b match {
              case BitNot(bb) => (b :: r._1, r._2 + b, r._3 + bb, r._4)
              case _ => (b :: r._1, r._2 + b, r._3, r._4)
            }
          }
      }
      if(cleanBits._4 == ZERO) ZERO
      else if(cleanBits._1.size == 1) cleanBits._1.head
      else new BitAnd(cleanBits._1)
    }

  // TODO: solve this with the type system
  private[bitwise] def cleanApply(bits: List[Bit]): Bit = new BitAnd(bits)
}

case class BitAnd[T <: Bit] (bits: List[T]) extends BitFormula with AssociativeOperator {
  private[bitwise] val opSymbol = "&"
  def substitute(vars: Map[BitVar, Bit]): Bit = BitAnd(bits.map(_.substitute(vars)))
  def op: (Bit, Bit) => Bit = _ & _
}

object BitOr {
  def apply[T <: Bit] (bits: List[T]): Bit =
    if(bits.exists(_ == ONE)) ONE
    else if(bits.forall(_ == ZERO)) ZERO
    else {
      val cleanBits = bits.flatMap(
        bit => bit match {
          case or: BitOr[_] => or.bits
          case ZERO => Nil
          case bit => List(bit)
        }
      ).foldRight((Nil: List[Bit], Set(): Set[Bit], Set(): Set[Bit], ZERO)) { // TODO: break loop earlier
        (l, r) =>
          l match {
            case BitNot(b) if(r._2.contains(b)) => (r._1, r._2, r._3, ONE)
            case b if(r._3.contains(b)) => (r._1, r._2, r._3, ONE)
            case b if(r._2.contains(b)) => r
            case b => b match {
              case BitNot(bb) => (b :: r._1, r._2 + b, r._3 + bb, r._4)
              case _ => (b :: r._1, r._2 + b, r._3, r._4)
            }
          }
      }
      if(cleanBits._4 == ONE) ONE
      else if(cleanBits._1.size == 1) cleanBits._1.head
      else new BitOr(cleanBits._1)


    }
  // TODO: solve this with the type system
  private[bitwise] def cleanApply(bits: List[Bit]): Bit = new BitOr(bits)
}

case class BitOr[T <: Bit] (bits: List[T]) extends BitFormula with AssociativeOperator {
  private[bitwise] val opSymbol = "|"
  def substitute(vars: Map[BitVar, Bit]): Bit = BitOr(bits.map(_.substitute(vars)))
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
    val cleanBits = bits.flatMap(
      _ match {
        case xor: BitXor[_] => xor.bits
        case _: BitValue => Nil
        case bit => List(bit)
      }
    )
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
  def substitute(vars: Map[BitVar, Bit]): Bit = BitXor(bits.map(_.substitute(vars)))
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
  def substitute(vars: Map[BitVar, Bit]): Bit = left.substitute(vars) <-> right.substitute(vars)
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
  def substitute(vars: Map[BitVar, Bit]): Bit = !bit.substitute(vars)
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

case class BitVar (name: String) extends BitFormula with Atomic {
  override def toString: String = name
  def substitute(vars: Map[BitVar, Bit]): Bit = vars.getOrElse(this, this)
}