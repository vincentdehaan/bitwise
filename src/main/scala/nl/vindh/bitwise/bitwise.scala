package nl.vindh

package object bitwise {
  type Valuation = Map[BitVar, Bit]

  val ZERO = new BitValue(false)
  val ONE = new BitValue(true)
  val WORD_SIZE = 8

  implicit def lazyAdderConverter(la: LazyAdder): BitSequence = la.execute
}