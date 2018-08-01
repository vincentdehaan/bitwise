package nl.vindh.bitwise

import types._

object Hello extends App {
  class Seqq(val elts: List[Int]) extends scala.collection.immutable.LinearSeq[Int] {
    def apply(idx: Int): Int = elts(idx)
    def length: Int = elts.length
  }

  val lst = List(1,2,3)

  val s = new Seqq(lst)
  //println(1)
  println(s.toString)


}