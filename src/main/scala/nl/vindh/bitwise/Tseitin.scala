package nl.vindh.bitwise

object Tseitin {
  /*def transform(bit: Bit)(implicit helper: TseitinHelper): Set[BitOrList] = {
    def transformRec(bit: Bit)(implicit helper: TseitinHelper): (Option[BitVar], Set[BitOrList]) = bit match {
      case v: Atomic => (None, Set(new BitOrList(Set(v))))
      case bin: BinaryOperator => {
        val tseitinLeft = transformRec(bin.left)
        val tseitinRight = transformRec(bin.right)

      }
    }
  }*/

  /*
   * Returns the CNF of bit <-> op(left, right), where op is derived from formula
   */
  private[bitwise] def tseitinBinaryTemplate(formula: Bit, bit: Bit, left: BitVar, right: BitVar): Bit = formula match { // cache vals
    case and: BitAnd => (!bit | left) & (!bit | right) & (!left | !right | bit)
    case or: BitOr => (!bit | left | right) & (bit | !left) & (bit | !right)
    //case xor: BitXor =>
    //case eq: BitEq =>
  }
}

class TseitinHelper(val prefix: String) {

}