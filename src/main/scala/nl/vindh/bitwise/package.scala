package nl.vindh.bitwise


package object types {
  type BitSequence = IndexedSeq[Bit]

  val ZERO = new BitValue(0)
  val ONE = new BitValue(1)
  val WORD_SIZE = 8
}