package nl.vindh.bitwise.util

import nl.vindh.bitwise.Bit

object DimacsWriter {
  def cnfToDimacs(cnf: Bit): String = ???
}

trait DimacsLine {
  def text: String
}

case class DimacsFile(comments: List[DimacsComment], problem: DimacsProblem, clauses: List[DimacsClause])

case class DimacsComment(str: String) extends DimacsLine {
  override def text: String = s"c $str"
}

case class DimacsProblem(vars: Int, clauses: Int) extends DimacsLine {
  override def text: String = s"p cnf $vars $clauses"
}

case class DimacsClause(elts: List[Int]) extends DimacsLine{
  override def text: String = elts.mkString(" ") + " 0"
}