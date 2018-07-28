package nl.vindh.bitwise

import types._

class BitSequence private(val bits: Seq[Bit]) extends IndexedSeq[Bit]{ // TODO: why does toString give stackoverflow if I change the base class to LinearSeq?
  def apply(idx: Int): Bit = bits(idx)
  def length: Int = bits.length

  def toInt: Int = bits.zipWithIndex.map{
    _ match {
      case (bit: BitValue, exp: Int) => bit.value * scala.math.pow(2, exp).toInt
      case _ => throw new Exception("toInt only possible with sequence of values")
    }
  }.sum

  private def binOp(that: BitSequence, op: (Bit, Bit) => Bit): BitSequence =
    if(this.length != that.length) throw new Exception("Sequences not of same length")
    else new BitSequence(
      this.bits.zip(that.bits).map{
        case (left, right) => op(left, right)
      }
    )

  def & (that: BitSequence): BitSequence = binOp(that, _ & _)

  def | (that: BitSequence): BitSequence = binOp(that, _ | _)

  def ^ (that: BitSequence): BitSequence = binOp(that, _ ^ _)

  def unary_! : BitSequence = new BitSequence(this.bits.map(! _))

  def >>> (rot: Int): BitSequence = new BitSequence(bits.drop(rot) ++ bits.take(rot))

  def + (that: BitSequence): BitSequence =  // Implement a ripple-carry adder
    if(this.length != that.length) throw new Exception("Sequences not of same length")
    else new BitSequence(this.bits.zip(that.bits).foldLeft[(List[Bit], Bit)]((Nil, ZERO)) {
      (acc: (List[Bit], Bit), pair: (Bit, Bit)) => (acc, pair) match {
          case ((lst: List[Bit], carry: Bit), (left: Bit, right: Bit)) =>
            (((left ^ right) ^ carry) :: lst, ((left ^ right) & carry) | left & right)
        }
      }._1.reverse)
}

object BitSequence {
  def apply(i: Int): BitSequence = apply(i, WORD_SIZE)

  def apply(i: Int, size: Int): BitSequence =
    new BitSequence(
      0 until size map (n => if((i & (1 << n)) != 0) ONE else ZERO),
    )
}