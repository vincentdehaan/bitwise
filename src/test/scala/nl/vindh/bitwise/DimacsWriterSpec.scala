package nl.vindh.bitwise.util
// TODO: move file to util dir
import org.scalatest._
import nl.vindh.bitwise._

class DimacsWriterSpec extends FlatSpec with Matchers with BitVarXs {
  "DimacsWriter.cnfToDimacs" should "convert a CNF into Dimacs" in {
    // Arrange
    val cnf = (x1 | x2) & !x3 & x4

    // Act
    val (dimacsFile, _) = DimacsWriter.cnfToDimacs(cnf)
    val dimacsStr = dimacsFile.text

    // Assert
    assert(dimacsStr ===
    """
      |p cnf 4 3
      |1 2 0
      |-3 0
      |4 0""".stripMargin)
  }
}
