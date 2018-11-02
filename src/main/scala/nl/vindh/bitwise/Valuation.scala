package nl.vindh.bitwise

object Valuation {
  def apply(value: Int, size: Int, prefix: String): Valuation =
    // TODO: why are the outermost () necessary?
    (0 until size map {
      i => BitVar(prefix + i) -> (if ((value & (1 << i)) != 0) ONE else ZERO)
    }).toMap

  def apply(value: Int, prefix: String): Valuation =
    apply(value, WORD_SIZE, prefix)
}
