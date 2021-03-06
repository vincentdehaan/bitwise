package nl.vindh.bitwise

object Tseitin {
  def transform(bit: Bit)(implicit vargen: VariableGenerator): Bit = {
    val (clauses, varOption) = getClauses(bit)
    BitAnd(varOption :: clauses.map(clause => clause.toCnf))
  }

  def transform(seq: BitSequence)(implicit vargen: VariableGenerator): BitSequence =
    //new BitSequence(seq.map(transform(_)))
    new BitSequence(seq.bits.map(transform(_)))

  def getClauses(f: Bit)(implicit vargen: VariableGenerator): (List[TseitinClause], Bit) = {
    def handleAssociativeOperator(operands: List[Bit], apply: List[Bit] => Bit): (List[TseitinClause], Bit) = {
      operands match {
        case hd :: elt :: Nil => handleAssociativeOperatorWith2Operands(operands, apply)
        case hd :: tl => handleAssociativeOperatorWith2Operands(List(hd, apply(tl)), apply)
        case Nil => throw new Exception("Operator has not been constructed using factory method")
      }
    }

    def handleAssociativeOperatorWith2Operands(operands: List[Bit], apply: List[Bit] => Bit): (List[TseitinClause], Bit) = {
      val (clauseListList, varList) = operands.map(f => getClauses(f)).unzip
      val newVar = vargen.next
      (
        TseitinClause(
          newVar,
          apply(varList)
        ) :: clauseListList.flatten,
        newVar
      )
    }

    f match {
      case v: BitVar => (Nil, v)
      case v: BitValue => (Nil, v)
      case BitNot(n) => {
        val (clauses, varr) = getClauses(n)
        val newVar = vargen.next
        (
          TseitinClause(newVar, BitNot(varr)) :: clauses,
          newVar
        )
      }
      case BitAnd(lst) => handleAssociativeOperator(lst, BitAnd.apply)
      case BitOr(lst) => handleAssociativeOperator(lst, BitOr.apply)
      case BitXor(lst) => handleAssociativeOperator(lst, BitXor.apply)
      case BitEq(left, right) => {
        val (clauseListLeft, varLeft) = getClauses(left)
        val (clauseListRight, varRight) = getClauses(right)
        val newVar = vargen.next
        (
          TseitinClause(
            newVar,
            BitEq(varLeft, varRight)
          ) :: clauseListLeft ::: clauseListRight,
          newVar
        )
      }
    }
  }
}


case class TseitinClause (v: BitVar, f: Bit) {
  // Note that only v => f is implemented; see Kroening, p. 14. // TODO: I temporarily disabled this optimization because it conserves satisfiability, but the satisfying model changes
  //def toCnf: Bit = (!v | f).onlyAndOrNot.pushNotInside.pushOrInside
  def toCnf: Bit = (!v | f).onlyAndOrNot.pushNotInside.pushOrInside & (!f | v).onlyAndOrNot.pushNotInside.pushOrInside
}