package nl.vindh.bitwise

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class BitSequence (val bits: Seq[Bit]) extends IndexedSeq[Bit]{ // TODO: immutable?
  def apply(idx: Int): Bit = bits(idx)
  def map(f: Bit => Bit): BitSequence = new BitSequence(bits.map(f)) // TODO: this is not necessary if I can extend LinearSeq
  val length: Int = bits.length
  override def toString: String = bits.reverse.mkString("(", ",", ")")
  def toString(width: Int): String = bits.reverse.map{
    bit => {
      val s = bit.toString
      if(s.size < width) s + " " * (width - s.size)
      else if(s.size > width) s.substring(0, width)
      else s
    }
  }.mkString(" ")

  def toInt: Int = bits.zipWithIndex.map{
    _ match {
      case (bit: BitValue, exp: Int) => (if(bit.value) 1 else 0) * scala.math.pow(2, exp).toInt
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

  def <-> (that: BitSequence): BitSequence = binOp(that, _ <-> _)

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

  def || (that: BitSequence): BitSequence = new BitSequence(this.bits ++ that.bits)

  def || (that: Bit): BitSequence = new BitSequence(this.bits ++ Seq(that))

  def |>| (that: BitSequence): BitSequence = new BitSequence(this.bits ++ that.bits.reverse)

  // TODO: if I use map instead of bits.map, I get a stackoverflow error
  def substitute(vars: Map[BitVar, Bit]): BitSequence = new BitSequence(bits.map(bit => bit.substitute(vars)))
}

object BitSequence {
  def apply(i: Int): BitSequence = apply(i, WORD_SIZE)

  def apply(i: Int, size: Int): BitSequence =
    new BitSequence(
      0 until size map (n => if((i & (1 << n)) != 0) ONE else ZERO),
    )

  def apply(hex: String): BitSequence =
    new BitSequence(
      hex.grouped(2).toList.reverse.map(Integer.parseInt(_, 16)).map(i => 0 until 8 map(n => if((i & (1 << n)) != 0) ONE else ZERO)).flatten.toList
    )

  val empty: BitSequence = new BitSequence(Nil)

  def fromAscii(str: String): BitSequence =
    str.map(ch => BitSequence(ch.toInt, 8)).foldLeft(BitSequence.empty)(_ |>| _)

  def variable(prefix: String, size: Int) = new BitSequence(
    0 until size map (n => BitVar(prefix + n))
  )
// TODO: fix this
  def fromSeq(seq: Seq[Bit]): BitSequence = new BitSequence(seq)

  def newBuilder: mutable.Builder[Bit, BitSequence] = new ArrayBuffer mapResult fromSeq

  implicit def canBuildFrom: CanBuildFrom[IndexedSeq[Bit], Bit, BitSequence] =
    new CanBuildFrom[IndexedSeq[Bit], Bit, BitSequence] {
      def apply(): mutable.Builder[Bit, BitSequence] = newBuilder
      def apply(from: IndexedSeq[Bit]): mutable.Builder[Bit, BitSequence] = newBuilder
    }
}
