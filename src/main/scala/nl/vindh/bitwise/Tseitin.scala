package nl.vindh.bitwise

object Tseitin {
  def transform(bit: Bit): Bit = {
    val (clauses, varOption) = getClauses(bit)
    BitAnd(varOption :: clauses.map(clause => clause.toCnf))
  }

  def transform(seq: BitSequence): BitSequence =
    new BitSequence(seq.map(transform(_)))

  // TODO: start over for every transformation
  val varPrefix = "tstn"
  var varCounter = 0
  def getNewVar: BitVar = {
    varCounter = varCounter + 1
    BitVar(varPrefix + varCounter)
  }

  def getClauses(f: Bit): (List[TseitinClause], Bit) = {
    def handleAssociativeOperator(operands: List[Bit], apply: List[Bit] => Bit): (List[TseitinClause], Bit) = {
      val (clauseListList, varOptionList) = operands.map(f => getClauses(f)).unzip
      val newVar = getNewVar
      (
        TseitinClause(
          newVar,
          apply(
            varOptionList.zip(operands)
              .map {
                case (opt, bit) => opt
              }
          )
        ) :: clauseListList.flatten,
        newVar
      )
    }

    f match {
      case v: BitVar => (Nil, v)
      case v: BitValue => (Nil, v)
      case BitNot(n) => {
        val (clauses, varOption) = getClauses(n)
        val newVar = getNewVar
        (
          TseitinClause(newVar, BitNot(varOption)) :: clauses,
          newVar
        )
      }
      case BitAnd(lst) => handleAssociativeOperator(lst, BitAnd.apply)
      case BitOr(lst) => handleAssociativeOperator(lst, BitOr.apply)
      case BitXor(lst) => handleAssociativeOperator(lst, BitXor.apply)
      case BitEq(left, right) => {
        val (clauseListLeft, varOptionLeft) = getClauses(left)
        val (clauseListRight, varOptionRight) = getClauses(right)
        val newVar = getNewVar
        (
          TseitinClause(
            newVar,
            BitEq(varOptionLeft, varOptionRight)
          ) :: clauseListLeft ::: clauseListRight,
          newVar
        )
      }

    }
  }
}


case class TseitinClause (v: BitVar, f: Bit) {
  // Note that only v => f is implemented; see Kroening, p. 14.
  def toCnf: Bit = (!v | f).onlyAndOrNot.pushNotInside.pushOrInside
}