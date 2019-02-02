package nl.vindh.bitwise

import scala.collection.mutable

case class SimplifierConfig(
  minimumOperatorCount: Int = 10,
  substituteAll: Boolean = false
)

class VariableGenerator(prefix: String){
  private var i = -1
  def next: BitVar = {
    i = i + 1
    BitVar(s"$prefix$i")
  }
}

object Simplifier {
  def substituteAll(bit: Bit)(implicit vargen: VariableGenerator): (Bit, Defs) =
    bit match {
      case _: BitVar => (bit, Map[BitVar, Bit]())
      case _ => {
        val newVar = vargen.next
        (newVar, Map(newVar -> bit))
      }
    }

  def substituteAll(bs: BitSequence)(implicit vargen: VariableGenerator): (BitSequence, Defs) = {
    val tuples = bs.bits.map {
      case v: BitVar => (v -> v)
      case v: BitValue => (v -> v)
      case b: Bit => {
          val newVar = vargen.next
          newVar -> b
        }
    }
    val bits = tuples.map { case (v, _) => v}
    val defs = tuples.filter {
      case (_, _: BitVar) => false
      case (_, _: BitValue) => false
      case _ => true
    }
    (new BitSequence(bits), defs.toMap.asInstanceOf[Defs]) // TODO: the type inferencer does not understand that all non-variable bits are filtered out
  }

  def substituteSubtrees(bit: Bit)(implicit vargen: VariableGenerator, config: SimplifierConfig): (Bit, Defs) = {
    val bs = BitSequence.fromSeq(List(bit))
    val (bs2, defs) = substituteSubtrees(bs)
    (bs2.bits(0), defs)
  }

  def substituteSubtrees(bs: BitSequence)(implicit vargen: VariableGenerator, config: SimplifierConfig): (BitSequence, Defs) = {
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

    val threshold = if(config.substituteAll) 0 else 1

    val expensiveSubtrees = subtrees.filter {
      case (bit, (ops, occ)) => ops * (occ - threshold) >= config.minimumOperatorCount
    }
//println(expensiveSubtrees.mkString("\n"))
    val defs = mutable.Map[BitVar, Bit]()

    val newTrees = expensiveSubtrees.toList.sortBy {
      case (_, (ops, occ)) => -ops * (occ - threshold)
    }.foldLeft(bs.bits){
      (trees, subtreeWithMetadata) => {
        val v = vargen.next
        defs(v) = subtreeWithMetadata._1
        trees.map(tree => replaceSubtree(tree, subtreeWithMetadata._1, v)) // TODO: this could be optimized if I keep track of the trees that actually contain the specific subtree
      }
    }

    (BitSequence.fromSeq(newTrees), defs.toMap)
  }

  def getRelevantDefs(f: Bit, defs: Defs): Defs = {
    val relevantDefs = scala.collection.mutable.Map[BitVar, Bit]()
    traverseSubtrees(f){
      case v: BitVar => defs.get(v) match {
        case Some(d) => {
          relevantDefs += (v -> d)
          relevantDefs ++= getRelevantDefs(d, defs)
        }
        case _ =>
      }
      case _ =>
    }
    relevantDefs.toMap
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
