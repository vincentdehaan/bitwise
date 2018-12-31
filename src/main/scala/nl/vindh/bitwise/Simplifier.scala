package nl.vindh.bitwise

import scala.collection.mutable

case class FormulaWithDefs(bit: Bit, defs: Map[BitVar, Bit]){
  def substitute(vars: Valuation): Bit = {
    val substitutedDefs = defs.map(tup => (tup._1, tup._2.substitute(vars)))
    val allVars = substitutedDefs ++ vars
    bit.substitute(allVars)
  }
}

case class SimplifierConfig(
  minimumOperatorCount: Int = 10
)

class VariableGenerator(prefix: String){
  private var i = -1
  def next: BitVar = {
    i = i + 1
    BitVar(s"$prefix$i")
  }
}

object Simplifier {
  def substituteSubtrees(bit: Bit)(implicit vargen: VariableGenerator, config: SimplifierConfig): FormulaWithDefs = {
    // Find all subtrees and register for each one the number of operands and the number of occurrences
    val subtrees = mutable.Map[Bit, (Int, Int)]()

    traverseSubtrees(bit){
      b => subtrees.get(b) match {
        case Some((ops, occ)) => subtrees(b) = (ops, occ + 1)
        case None => subtrees(b) = (countOperators(b), 1)
      }
    }

    val expensiveSubtrees = subtrees.filter {
      case (bit, (ops, occ)) => ops * (occ - 1) >= config.minimumOperatorCount // TODO: make 10 configurable
    }

    val defs = mutable.Map[BitVar, Bit]()

    val newTree = expensiveSubtrees.foldLeft(bit){
      (tree, subtreeWithMetadata) => {
        val v = vargen.next
        defs(v) = subtreeWithMetadata._1
        replaceSubtree(tree, subtreeWithMetadata._1, v)
      }
    }

    FormulaWithDefs(newTree, defs.toMap)
  }

  def traverseSubtrees(bit: Bit)(f: Bit => Unit): Unit = {
    f(bit)
    bit match {
      case BitValue(_) =>
      case BitVar(_) =>
      case op: BinaryOperator => {
        traverseSubtrees(op.left)(f)
        traverseSubtrees(op.right)(f)
      }
      case op: AssociativeOperator => op.bits.foreach(traverseSubtrees(_)(f))
      case BitNot(x) => traverseSubtrees(x)(f)
      case _ => throw new Exception("Unknown tree type")
    }
  }

  // TODO: I could optimize this with Option, returning None of nothing was replaced
  def replaceSubtree(tree: Bit, subtree: Bit, replacement: Bit): Bit = {
    if(tree == subtree) replacement
    else tree match {
      case BitValue(_) => tree
      case BitVar(_) => tree
      case BitEq(left, right) =>
        BitEq(replaceSubtree(left, subtree, replacement), replaceSubtree(right, subtree, replacement))
      case BitAnd(lst) => BitAnd(lst.map(replaceSubtree(_, subtree, replacement)))
      case BitOr(lst) => BitOr(lst.map(replaceSubtree(_, subtree, replacement)))
      case BitXor(lst) => BitXor(lst.map(replaceSubtree(_, subtree, replacement)))
      case BitNot(x) => BitNot(replaceSubtree(x, subtree, replacement))
      case _ => throw new Exception("Unknown tree type")
    }
  }

  // TODO: can this be used in the Metrics object or vice versa?
  def countOperators(bit: Bit): Int = {
    bit match {
      case BitValue(_) => 0
      case BitVar(_) => 0
      case op: BinaryOperator =>
        countOperators(op.left) + countOperators(op.right) + 1
      case op: AssociativeOperator => op.bits.map(countOperators(_)).sum + 1
      case BitNot(x) => countOperators(x) + 1
      case _ => throw new Exception("Unknown tree type")
    }
  }
}
