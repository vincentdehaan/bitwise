package nl.vindh.bitwise.util

import nl.vindh.bitwise.{BitSequence, BitVar, ONE, Tseitin, VariableGenerator, ZERO}
import org.sat4j.core.VecInt
import org.sat4j.minisat.SolverFactory
import org.sat4j.specs.IProblem

object Sat4JWrapper {
  def solveForInt(eq: Equation)(implicit len: VarLength): Option[Int] = {
    implicit val vargen = new VariableGenerator("t")
    val cnf = Tseitin.transform(eq.bits)
    val cnf2 = cnf.bits.fold(ONE)((l, r) => l & r)
    val (file, vars) = DimacsWriter.cnfToDimacs(cnf2)
    val solver = SolverFactory.newDefault
    solver.setTimeout(60)
    solver.newVar(file.problem.vars)
    solver.setExpectedNumberOfClauses(file.problem.clauses)
    file.clauses.elts.foreach {
      clause =>
        solver.addClause(new VecInt(clause.toArray))
    }

    val problem: IProblem = solver

    if(problem.isSatisfiable) {
      val valuation = problem.model.map(i => if(i > 0) getVar(vars, i) -> ONE else getVar(vars, -i) -> ZERO).toMap
      val eqs = eq.bits.substitute(valuation)
      val x = BitSequence.variable("x", len.x)
      Some(x.substitute(valuation).toInt)
    } else
      None
  }
  def getVar(vars: Map[BitVar, Int], v: Int): BitVar =
    vars.find(tup => tup._2 == v).get._1
}

// Models the equation f(x) = v
case class Equation(f: BitSequence => BitSequence, v: Int)(implicit len: VarLength) {
  val bits = f(BitSequence.variable("x", len.x)) <-> BitSequence.apply(v, len.x, false)
}

case class VarLength(x: Int)