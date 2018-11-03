package nl.vindh.bitwise

class BitSequence (val bits: Seq[Bit]){//} extends scala.collection.immutable.LinearSeq[Bit]{//IndexedSeq[Bit]{ // TODO: why does toString give stackoverflow if I change the base class to LinearSeq?
  def apply(idx: Int): Bit = bits(idx)
  def map(f: Bit => Bit): BitSequence = new BitSequence(bits.map(f)) // TODO: this is not necessary if I can extend LinearSeq
  def length: Int = bits.length
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
  // TODO: if I use map instead of bits.map, I get a stackoverflow error
  def substitute(vars: Map[BitVar, Bit]): BitSequence = new BitSequence(bits.map(bit => bit.substitute(vars)))
}

object BitSequence {
  def apply(i: Int): BitSequence = apply(i, WORD_SIZE)

  def apply(i: Int, size: Int): BitSequence =
    new BitSequence(
      0 until size map (n => if((i & (1 << n)) != 0) ONE else ZERO),
    )

  def variable(prefix: String, size: Int) = new BitSequence(
    0 until size map (n => BitVar(prefix + n))
  )
}
