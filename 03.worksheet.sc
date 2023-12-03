/*
--- Day 3: Gear Ratios ---
You and the Elf eventually reach a gondola lift station; he says the gondola
lift will take you up to the water source, but this is as far as he can bring
you. You go inside.

It doesn't take long to find the gondolas, but there seems to be a problem:
  they're not moving.

"Aaah!"

You turn around to see a slightly-greasy Elf with a wrench and a look of
surprise. "Sorry, I wasn't expecting anyone! The gondola lift isn't working
right now; it'll still be a while before I can fix it." You offer to help.

The engineer explains that an engine part seems to be missing from the engine,
but nobody can figure out which one. If you can add up all the part numbers in
the engine schematic, it should be easy to work out which part is missing.

The engine schematic (your puzzle input) consists of a visual representation of
the engine. There are lots of numbers and symbols you don't really understand,
but apparently any number adjacent to a symbol, even diagonally, is a "part
number" and should be included in your sum. (Periods (.) do not count as a
symbol.)

Here is an example engine schematic:

467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..

In this schematic, two numbers are not part numbers because they are not
adjacent to a symbol: 114 (top right) and 58 (middle right). Every other number
is adjacent to a symbol and so is a part number; their sum is 4361.

Of course, the actual engine schematic is much larger. What is the sum of all of
the part numbers in the engine schematic?

--- Part Two ---
The engineer finds the missing part and installs it in the engine! As the engine
springs to life, you jump in the closest gondola, finally ready to ascend to the
water source.

You don't seem to be going very fast, though. Maybe something is still wrong?
Fortunately, the gondola has a phone labeled "help", so you pick it up and the
engineer answers.

Before you can explain the situation, she suggests that you look out the window.
There stands the engineer, holding a phone in one hand and waving with the
other. You're going so slowly that you haven't even left the station. You exit
the gondola.

The missing part wasn't the only issue - one of the PartsAndGears in the engine is
wrong. A gear is any * symbol that is adjacent to exactly two part numbers. Its
gear ratio is the result of multiplying those two numbers together.

This time, you need to find the gear ratio of every gear and add them all up so
that the engineer can figure out which gear needs to be replaced.

Consider the same engine schematic again:

467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..

In this schematic, there are two PartsAndGears. The first is in the top left; it has
part numbers 467 and 35, so its gear ratio is 16345. The second gear is in the
lower right; its gear ratio is 451490. (The * adjacent to 617 is not a gear
because it is only adjacent to one part number.) Adding up all of the gear
ratios produces 467835.

What is the sum of all of the gear ratios in your engine schematic?
 */
object DataDefs:
  type X = Int; type Y = Int
  case class Interval(start: Point, end: Point) // a number's start-end positions
  case class Point(x: X, y: Y):
    def isNear(i: Interval): Boolean = // is (x, y) near a number? including diagonally
      i.start.x - 1 <= x && x <= i.end.x + 1 && i.start.y - 1 <= y && y <= i.end.y + 1

  type PartNumber = Int // sum these for part 1
  case class Part(partNumber: PartNumber, interval: Interval) // a number and its position

  case class Gear(p1: Part, p2: Part): // a gear is near two parts
    val ratio: Int = p1.partNumber * p2.partNumber // sum these for part 2

object Parsing:
  import DataDefs.*
  extension (c: Char)
    def isSymbol: Boolean = !(c.isDigit || c.isLetter || c == '.' || c == ' ')

  def finder(pred: Char => Boolean)(lines: Seq[String]): Seq[Point] =
    for
      (line, y) <- lines.zipWithIndex
      (c, x) <- line.zipWithIndex
      if pred(c)
    yield Point(x, y)
  val findAllSymbols = finder(_.isSymbol) // part 1
  val findAsterisks = finder(_ == '*') // part 2

  def findNumbersInLine(y: Y, line: String): Seq[Part] =
    val matches = "[0-9]+".r.findAllIn(line)
    val numbers = collection.mutable.Queue.empty[Part]
    while matches.hasNext do
      val number = matches.next()
      val start = Point(matches.start, y)
      val end = Point(matches.end - 1, y) // this index is off-by-1, so fix it.
      val part = Part(number.toInt, Interval(start, end))
      numbers.append(part)
    numbers.toSeq

object PartsAndGears:
  import DataDefs.*, Parsing.*
  def findAllParts(lines: Seq[String], symbols: Seq[Point]): Seq[Part] =
    for
      (line, y) <- lines.zipWithIndex
      part <- findNumbersInLine(y, line)
      if symbols.exists(_.isNear(part.interval))
    yield part

  def findGear(asterisk: Point, parts: Seq[Part]): Option[Gear] =
    val nearbyParts = parts.filter(part => asterisk.isNear(part.interval))
    nearbyParts.length match
      case 2 => Some(Gear(nearbyParts(0), nearbyParts(1)))
      case _ => None

  def findAllGears(lines: Seq[String], symbols: Seq[Point]): Seq[Gear] =
    val asterisks = findAsterisks(lines)
    val parts = findAllParts(lines, symbols)
    asterisks.flatMap(findGear(_, parts))

object Summing:
  import PartsAndGears.*, Parsing.*
  def sumPartNumbers(lines: Seq[String]): Int = // for part 1
    val symbols = findAllSymbols(lines)
    val parts = findAllParts(lines, symbols)
    parts.map(_.partNumber).sum

  def sumGearRatios(lines: Seq[String]): Int = // for part 2
    val symbols = findAllSymbols(lines)
    val gears = findAllGears(lines, symbols)
    gears.map(_.ratio).sum

object Testing:
  import Summing.*
  val testInput = """
  |467..114..
  |...*......
  |..35..633.
  |......#...
  |617*......
  |.....+.58.
  |..592.....
  |......755.
  |...$.*....
  |.664.598..""".stripMargin

  val lines = testInput.split("\n").toSeq
  val testResult1 = sumPartNumbers(lines)
  val testResult2 = sumGearRatios(lines)
Testing.testResult1 // part 1: 4361
Testing.testResult2 // part 2: 467835

object Main:
  import Summing.*
  val lines: Seq[String] = os.read.lines(os.pwd / "03.input.txt")
  val result1 = sumPartNumbers(lines)
  val result2 = sumGearRatios(lines)
Main.result1 // Part 1: 514969
Main.result2 // Part 2: 78915902
