package nl.vindh

package object bitwise {
  type Valuation = Map[BitVar, BitValue]
  type Defs = Map[BitVar, Bit]

  val ZERO = new BitValue(false)
  val ONE = new BitValue(true)
  val WORD_SIZE = 8

  implicit class BitSequenceWithDefs(self: (BitSequence, Defs)){
    def +/ (that: BitSequence)(implicit vargen: VariableGenerator): (BitSequence, Defs) = self match {
      case (bs1, defs1) =>
        val (bs2, defs2) = bs1 +/ that
        (bs2, defs1 ++ defs2)
    }
  }
}