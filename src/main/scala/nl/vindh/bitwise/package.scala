package nl.vindh.bitwise


package object types {
  type BitSequence = IndexedSeq[Bit]

  val ZERO = new BitValue(false)
  val ONE = new BitValue(true)
  val WORD_SIZE = 8
}