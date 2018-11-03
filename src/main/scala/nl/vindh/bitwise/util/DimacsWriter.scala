package nl.vindh.bitwise.util

import nl.vindh.bitwise._

object DimacsWriter {
  def cnfToDimacs(cnf: Bit): (DimacsFile, Map[BitVar, Int]) = {
    // TODO: eliminate ._1, ._2
    val variableMap = getVariables(cnf).zipWithIndex.map(x => (x._1, x._2 + 1)).toMap

    def andClauses(andList: List[Bit]): List[List[Int]] =
      andList.map {
        case BitOr(orList) => orClauses(orList)
        case v: BitVar => List(variableMap(v))
        case BitNot(v: BitVar) => List(-variableMap(v))
        case _ => throw new Exception("Unexpected format (*).")
      }

    def orClauses(orList: List[Bit]): List[Int] =
      orList.map {
        case v: BitVar => variableMap(v)
        case BitNot(v: BitVar) => -variableMap(v)
        case _ => throw new Exception("Unexpected format (**).")
      }

    val clauses = cnf match {
      case BitAnd(andList) => andClauses(andList)
      case BitOr(orList) => List(orClauses(orList))
      case v: BitVar => List(List(variableMap(v)))
      case BitNot(v: BitVar) => List(List(-variableMap(v)))
      case _ => throw new Exception("Unexpected format (***).")
    }

    (
      DimacsFile(
        Nil,
        DimacsProblem(
          variableMap.size,
          clauses.size
        ),
        DimacsClauses(clauses)
      ),
      variableMap
    )
  }

  private def getVariables(bit: Bit): Set[BitVar] = bit match {
    case bvar: BitVar => Set(bvar)
    case ass: AssociativeOperator => ass.bits.flatMap(b => getVariables(b)).toSet
    case BitNot(not) => getVariables(not)
    case _ => Set()
  }
}

trait DimacsFileElement {
  def text: String
}

case class DimacsFile(comments: List[DimacsComment], problem: DimacsProblem, clauses: DimacsClauses) extends DimacsFileElement {
  override def text: String = comments.map(_.text).mkString("\n") + problem.text + clauses.text
}

case class DimacsComment(str: String) extends DimacsFileElement {
  override def text: String = s"c $str"
}

// TODO: clauses seems to print a value that is much too large
case class DimacsProblem(vars: Int, clauses: Int) extends DimacsFileElement {
  override def text: String = s"p cnf $vars $clauses"
}

case class DimacsClauses(elts: List[List[Int]]) extends DimacsFileElement {
  override def text: String = elts.map(_.mkString(" ") + " 0").mkString("\n")
}