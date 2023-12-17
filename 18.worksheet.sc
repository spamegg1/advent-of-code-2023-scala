/*

--- Part Two ---
 */
object DataDefs:
  ???

object Parsing:
  import DataDefs.*
  ???

object Solving:
  def solve1(lines: Seq[String]): Int = 0
  def solve2(lines: Seq[String]): Int = 0

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "18.test.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Testing.result1 // part 1: ???
// Testing.result2 // part 2: ???

object Main:
  private lazy val lines = os.read.lines(os.pwd / "18.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Main.result1 // part 1: ???
// Main.result2 // part 2: ???
