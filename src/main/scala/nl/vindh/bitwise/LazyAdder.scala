package nl.vindh.bitwise

import scala.collection.mutable
import scala.math.log

class LazyAdder(val ops: List[BitSequence]) {
  def execute: BitSequence = addList(ops)//ops.tail.foldLeft(ops.head)((bsx, bsy) => bsx.simpleAdd(bsy))

  def addList(lst: List[BitSequence]): BitSequence = {
    val sameLength = lst.forall(bs => bs.bits.size == lst(0).bits.size)
    require(sameLength, "Cannot add BitSequences of different length")

    // TODO: solve this in a more functional way
    val cols = List.fill(lst(0).bits.size)(mutable.ListBuffer[Bit]())
    // Fill columns with operands
    lst.foreach {
      bs => bs.bits.zipWithIndex.foreach {
        case (bit, j) => {
          cols(j) += bit
        }
      }
    }

    // The ith carry bit is true if n & 2^i == 1 with n the number of bits that are true.
    def carryBit(bits: List[Bit], idx: Int): Bit = {
      BitOr(
        bits.zipWithIndex.toSet.subsets.filter { // TODO: I don't like using zipWithIndex here, but it is necessary to use the subsets method and treat different instances of the same value as different elements
          subset => (subset.size & scala.math.pow(2, idx).toInt) != 0
        }.map {
          set => BitAnd(set.toList.map(tup => tup._1))
        }.toList
      )
    }
    // Add carries to columns
    cols.zipWithIndex.foreach {
      case (col, colIdx) => {
        val sumSize = col.size // number of items to be added
        val carrySize = (log(sumSize) / log(2)).floor.toInt // TODO: won't this result in rounding errors?

        (1 to carrySize).foreach {
          carryIdx => {
            if(colIdx + carryIdx < cols.size) cols(colIdx + carryIdx) += carryBit(col.toList, carryIdx)
          }
        }
      }
    }

    BitSequence.fromSeq(cols.map(col => BitXor(col.toList)))
  }
}
