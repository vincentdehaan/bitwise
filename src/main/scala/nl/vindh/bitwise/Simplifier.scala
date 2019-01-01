package nl.vindh.bitwise

import scala.collection.mutable

case class BitWithDefs(bit: Bit, defs: Map[BitVar, Bit]){
  def substitute(vars: Valuation): Bit = {
    // TODO: what if multiple simplifications have resulted in nested substitutions?
    val substitutedDefs = defs.map(tup => (tup._1, tup._2.substitute(vars)))
    val allVars = substitutedDefs ++ vars
    bit.substitute(allVars)
  }
}

case class BitSequenceWithDefs(bs: BitSequence, defs: Map[BitVar, Bit]) extends BitSequence(bs.bits){
  override def constructor(bits: Seq[Bit]): BitSequence =
    new BitSequenceWithDefs(new BitSequence(bits), defs)

  override def substitute(vars: Valuation): BitSequenceWithDefs = {
    // TODO: what if multiple simplifications have resulted in nested substitutions?
    val substitutedDefs = defs.map(tup => (tup._1, tup._2.substitute(vars)))
    val allVars = substitutedDefs ++ vars
    BitSequenceWithDefs(super.substitute(allVars), substitutedDefs)
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
  def substituteSubtrees(bit: Bit)(implicit vargen: VariableGenerator, config: SimplifierConfig): BitWithDefs = {
    val bs = BitSequence.fromSeq(List(bit))
    val bswd = substituteSubtrees(bs)
    BitWithDefs(bswd.bs.bits(0), bswd.defs)
  }

  def substituteSubtrees(bs: BitSequence)(implicit vargen: VariableGenerator, config: SimplifierConfig): BitSequenceWithDefs = {
    // Find all subtrees and register for each one the number of operands and the number of occurrences
    val subtrees = mutable.Map[Bit, (Int, Int)]()

    bs.bits.foreach {
      bit => traverseSubtrees(bit) {
        b => subtrees.get(b) match {
          case Some((ops, occ)) => subtrees(b) = (ops, occ + 1)
          case None => subtrees(b) = (countOperators(b), 1)
        }
      }
    }

    val expensiveSubtrees = subtrees.filter {
      case (bit, (ops, occ)) => ops * (occ - 1) >= config.minimumOperatorCount // TODO: make 10 configurable
    }

    val defs = mutable.Map[BitVar, Bit]()

    val newTrees = expensiveSubtrees.foldLeft(bs.bits){ // TODO: shouldn't I sort this first in order to start with the most expensive replacement
      (trees, subtreeWithMetadata) => {
        val v = vargen.next
        defs(v) = subtreeWithMetadata._1
        trees.map(tree => replaceSubtree(tree, subtreeWithMetadata._1, v)) // TODO: this could be optimized if I keep track of the trees that actually contain the specific subtree
      }
    }

    BitSequenceWithDefs(BitSequence.fromSeq(newTrees), defs.toMap)
  }

  private def traverseSubtrees(bit: Bit)(f: Bit => Unit): Unit = {
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
  private def replaceSubtree(tree: Bit, subtree: Bit, replacement: Bit): Bit = {
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
  private def countOperators(bit: Bit): Int = {
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
