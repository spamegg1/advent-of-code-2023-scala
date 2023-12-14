/*
--- Day 14: Parabolic Reflector Dish ---
You reach the place where all of the mirrors were pointing: a massive parabolic
reflector dish attached to the side of another large mountain.

The dish is made up of many small mirrors, but while the mirrors themselves are
roughly in the shape of a parabolic reflector dish, each individual mirror seems
to be pointing in slightly the wrong direction. If the dish is meant to focus
light, all it's doing right now is sending it in a vague direction.

This system must be what provides the energy for the lava! If you focus the
reflector dish, maybe you can go where it's pointing and use the light to fix
the lava production.

Upon closer inspection, the individual mirrors each appear to be connected via
an elaborate system of ropes and pulleys to a large metal platform below the
dish. The platform is covered in large rocks of various shapes. Depending on
their position, the weight of the rocks deforms the platform, and the shape of
the platform controls which ropes move and ultimately the focus of the dish.

In short: if you move the rocks, you can focus the dish. The platform even has a
control panel on the side that lets you tilt it in one of four directions! The
rounded rocks (O) will roll when the platform is tilted, while the cube-shaped
rocks (#) will stay in place. You note the positions of all of the empty spaces
(.) and rocks (your puzzle input). For example:
O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....

Start by tilting the lever so all of the rocks will slide north as far as they
will go:
OOOO.#.O..
OO..#....#
OO..O##..O
O..#.OO...
........#.
..#....#.#
..O..#.O.O
..O.......
#....###..
#....#....

You notice that the support beams along the north side of the platform are
damaged; to ensure the platform doesn't collapse, you should calculate the total
load on the north support beams.

The amount of load caused by a single rounded rock (O) is equal to the number of
rows from the rock to the south edge of the platform, including the row the rock
is on. (Cube-shaped rocks (#) don't contribute to load.) So, the amount of load
caused by each rock in each row is as follows:
OOOO.#.O.. 10
OO..#....#  9
OO..O##..O  8
O..#.OO...  7
........#.  6
..#....#.#  5
..O..#.O.O  4
..O.......  3
#....###..  2
#....#....  1

The total load is the sum of the load caused by all of the rounded rocks. In
this example, the total load is 136.

Tilt the platform so that the rounded rocks all roll north. Afterward, what is
the total load on the north support beams?

--- Part Two ---
The parabolic reflector dish deforms, but not in a way that focuses the beam.
To do that, you'll need to move the rocks to the edges of the platform.
Fortunately, a button on the side of the control panel labeled "spin cycle"
attempts to do just that!

Each cycle tilts the platform four times so that the rounded rocks roll north,
then west, then south, then east. After each tilt, the rounded rocks roll as far
as they can before the platform tilts in the next direction. After one cycle,
the platform will have finished rolling the rounded rocks in those four
directions in that order.

Here's what happens in the example above after each of the first few cycles:
After 1 cycle:
.....#....
....#...O#
...OO##...
.OO#......
.....OOO#.
.O#...O#.#
....O#....
......OOOO
#...O###..
#..OO#....

After 2 cycles:
.....#....
....#...O#
.....##...
..O#......
.....OOO#.
.O#...O#.#
....O#...O
.......OOO
#..OO###..
#.OOO#...O

After 3 cycles:
.....#....
....#...O#
.....##...
..O#......
.....OOO#.
.O#...O#.#
....O#...O
.......OOO
#...O###.O
#.OOO#...O
This process should work if you leave it running long enough, but you're still
worried about the north support beams. To make sure they'll survive for a while,
you need to calculate the total load on the north support beams after 1000000000
cycles.

In the above example, after 1000000000 cycles, the total load on the north
support beams is 64.

Run the spin cycle for 1000000000 cycles. Afterward, what is the total load on
the north support beams?
 */
object DataDefs:
  type Rock = Char
  type Row = Vector[Rock]
  extension (row: Row)
    private def rollWestNoHashes: Row =
      val (ohs, dots) = row.partition(_ == 'O')
      ohs ++ dots

    @annotation.tailrec
    private def helper(current: Row)(acc: Row): Row =
      if current.isEmpty then acc
      else
        val (noHashes, tail) =
          (current.takeWhile(_ != '#'), current.dropWhile(_ != '#'))
        val (hashes, rest) = (tail.takeWhile(_ == '#'), tail.dropWhile(_ == '#'))
        val newAcc = acc ++ noHashes.rollWestNoHashes ++ hashes
        helper(rest)(newAcc)

    def west: Row = helper(row)(Vector())
    def east: Row = row.reverse.west.reverse

  type Platform = Vector[Row]
  extension (platform: Platform)
    private def cols: Platform = platform.transpose
    def rollNorth: Platform = cols.map(_.west).transpose
    private def rollSouth: Platform = platform.reverse.rollNorth.reverse
    private def rollWest: Platform = platform.map(_.west)
    private def rollEast: Platform = platform.map(_.east)
    def runCycle: Platform = platform.rollNorth.rollWest.rollSouth.rollEast
    def show: String = platform.map(_.mkString).mkString("\n")

object Parsing:
  import DataDefs.*
  def parsePlatform(file: String): Platform = file
    .split("\n")
    .map(_.toVector)
    .toVector

object Solving:
  import DataDefs.*
  private def loadOf(platform: Platform): Int = platform
    .zip(platform.size to 1 by -1)
    .foldLeft(0):
      case (total, (row, load)) => total + row.count(_ == 'O') * load

  private val memo = collection.mutable.Map[String, Int]()

  @annotation.tailrec
  private def findStableCycle(platform: Platform)(count: Int): (Int, Int) =
    def key = platform.show
    memo.get(key) match
      case None =>
        memo.addOne(key -> count)
        findStableCycle(platform.runCycle)(count + 1)
      case Some(value) => (value, count)

  private def findCycle(platform: Platform) = findStableCycle(platform)(0)

  private def runCycles(platform: Platform)(times: Int): Platform =
    var state = platform
    (0 until times).foreach(_ => state = state.runCycle)
    state

  def solve1(file: String) = loadOf(Parsing.parsePlatform(file).rollNorth)
  def solve2(file: String) =
    val platform = Parsing.parsePlatform(file)
    val (start, end) = findCycle(platform)
    val cycleSize = end - start
    val times = (1000000000 - start) % cycleSize + start
    loadOf(runCycles(platform)(times))

object Testing:
  import DataDefs.*
  private lazy val file = os.read(os.pwd / "14.test.input.txt")
  lazy val testResult1 = Solving.solve1(file)
  lazy val testResult2 = Solving.solve2(file)
Testing.testResult1 // part 1: 136
Testing.testResult2 // part 2: 64

object Main:
  private lazy val file = os.read(os.pwd / "14.input.txt")
  lazy val result1 = Solving.solve1(file)
  lazy val result2 = Solving.solve2(file)
Main.result1 // part 1: 110779
Main.result2 // part 2: 86069
