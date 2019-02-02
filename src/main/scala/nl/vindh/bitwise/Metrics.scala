package nl.vindh.bitwise

object Metrics {
  case class OperatorCount(
    ops: Int = 0,
    vars: Int = 0,
    vals: Int = 0,
    opTable: Map[String, Int] = Map()
  )

  private def mergeOpTables(tables: Iterable[Map[String, Int]]): Map[String, Int] = {
    tables.foldLeft(Map[String, Int]()){
      (acc, add) => (acc.keySet ++ add.keySet).map(key => key -> (acc.getOrElse(key, 0) + add.getOrElse(key, 0))).toMap
    }
  }

  private def mergeOperatorCount(c1: OperatorCount, c2: OperatorCount): OperatorCount =
    OperatorCount(
      ops = c1.ops + c2.ops,
      vars = c1.vars + c2.vars,
      vals = c1.vals + c2.vals,
      opTable = mergeOpTables(List(c1.opTable, c2.opTable))
    )

  def countOperators(bit: Bit): OperatorCount =
    bit match {
      case BitValue(_) => OperatorCount(vals = 1)
      case BitVar(_) => OperatorCount(vars = 1)
      case op: BinaryOperator => {
        val leftcount = countOperators(op.left)
        val rightcount = countOperators(op.right)
        OperatorCount(
          ops = leftcount.ops + rightcount.ops + 1,
          vars = leftcount.vars + rightcount.vars,
          vals = leftcount.vals + rightcount.vals,
          opTable = mergeOpTables(List(leftcount.opTable, rightcount.opTable, Map(op.opSymbol -> 1)))
        )
      }
      case op: AssociativeOperator => {
        val counts = op.bits.map(bit => countOperators(bit))
        OperatorCount(
          ops = counts.map(count => count.ops).sum + op.bits.size - 1,
          vars = counts.map(count => count.vars).sum,
          vals = counts.map(count => count.vals).sum,
          opTable = mergeOpTables(Map(op.opSymbol -> (op.bits.size - 1)) :: counts.map(count => count.opTable).toList)
        )
      }
      case BitNot(x) => {
        val count = countOperators(x)
        count.copy(
          ops = count.ops + 1,
          opTable = mergeOpTables(List(count.opTable, Map("!" -> 1)))
        )
      }
      case _ => OperatorCount(opTable = Map("UNKNOWN" -> 1))
    }

  def countOperators(bs: BitSequence): OperatorCount =
    bs.bits.map(countOperators(_)).foldLeft(OperatorCount())((acc, nw) => mergeOperatorCount(acc, nw))
}
