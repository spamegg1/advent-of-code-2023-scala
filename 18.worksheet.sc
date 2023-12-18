/*
--- Day 18: Lavaduct Lagoon ---
Thanks to your efforts, the machine parts factory is one of the first factories
up and running since the lavafall came back. However, to catch up with the large
backlog of parts requests, the factory will also need a large supply of lava for
a while; the Elves have already started creating a large lagoon nearby for this
purpose.

However, they aren't sure the lagoon will be big enough; they've asked you to
take a look at the dig plan (your puzzle input). For example:
R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
The digger starts in a 1 meter cube hole in the ground. They then dig the
specified number of meters up (U), down (D), left (L), or right (R), clearing
full 1 meter cubes as they go. The directions are given as seen from above, so
if "up" were north, then "right" would be east, and so on. Each trench is also
listed with the color that the edge of the trench should be painted as an RGB
hexadecimal color code.

When viewed from above, the above example dig plan would result in the following
loop of trench (#) having been dug out from otherwise ground-level terrain (.):
#######
#.....#
###...#
..#...#
..#...#
###.###
#...#..
##..###
.#....#
.######
At this point, the trench could contain 38 cubic meters of lava. However, this
is just the edge of the lagoon; the next step is to dig out the interior so that
it is one meter deep as well:
#######
#######
#######
..#####
..#####
#######
#####..
#######
.######
.######
Now, the lagoon can contain a much more respectable 62 cubic meters of lava.
While the interior is dug out, the edges are also painted according to the color
codes in the dig plan.

The Elves are concerned the lagoon won't be large enough; if they follow their
dig plan, how many cubic meters of lava could it hold?

--- Part Two ---
The Elves were right to be concerned; the planned lagoon would be much too small.

After a few minutes, someone realizes what happened; someone swapped the color
and instruction parameters when producing the dig plan. They don't have time to
fix the bug; one of them asks if you can extract the correct instructions from
the hexadecimal codes.

Each hexadecimal code is six hexadecimal digits long. The first five hexadecimal
digits encode the distance in meters as a five-digit hexadecimal number. The
last hexadecimal digit encodes the direction to dig: 0 means R, 1 means D,
2 means L, and 3 means U.

So, in the above example, the hexadecimal codes can be converted into the true
instructions:

    #70c710 = R 461937
    #0dc571 = D 56407
    #5713f0 = R 356671
    #d2c081 = D 863240
    #59c680 = R 367720
    #411b91 = D 266681
    #8ceee2 = L 577262
    #caa173 = U 829975
    #1b58a2 = L 112010
    #caa171 = D 829975
    #7807d2 = L 491645
    #a77fa3 = U 686074
    #015232 = L 5411
    #7a21e3 = U 500254

Digging out this loop and its interior produces a lagoon that can hold an
impressive 952408144115 cubic meters of lava.

Convert the hexadecimal color codes into the correct instructions; if the Elves
follow this new dig plan, how many cubic meters of lava could the lagoon hold?
 */
object DataDefs:
  enum Dir:
    case U, D, L, R
  import Dir.*
  extension (s: String)
    def toDir: Dir = s match
      case "U" => U
      case "D" => D
      case "L" => L
      case "R" => R

  type Color = String
  case class Dig(dir: Dir, size: Int, color: Color)

  type Ground = Char
  type Row = Int
  type Col = Int
  case class Pos(var row: Row, var col: Col, var ground: Ground):
    def dig: Unit = ground = '#'
    def shiftBy(r: Row, c: Col): Unit =
      row = row - r
      col = col - r

  enum State:
    case Outside, Entering, Inside, Leaving
  import State.*

  extension (row: Seq[Pos])
    def rayCast: Int =
      var col = 0
      var result = 0
      var prev = '.'
      // 0: Outside -> 1: Boundary -> 2: Inside -> 3: Boundary -> 4: Outside ...
      var state = Outside
      while col < row.size do
        val ground = row(col).ground
        (ground, state) match
          case ('#', Outside)  => result += 1; state = Entering
          case ('#', Entering) => result += 1
          case ('#', Inside)   => result += 1; state = Leaving
          case ('#', Leaving)  => result += 1; state = Outside
          case ('.', Outside)  =>
          case ('.', Entering) => result += 1; state = Inside
          case ('.', Inside)   => result += 1
          case ('.', Leaving)  => state = Outside
          case _               =>
        col += 1
      result

  case class Trench(dir: Dir, start: Pos, end: Pos):
    lazy val minRow = math.min(start.row, end.row)
    lazy val maxRow = math.max(start.row, end.row)
    lazy val minCol = math.min(start.col, end.col)
    lazy val maxCol = math.max(start.col, end.col)
    def shiftBy(row: Row, col: Col): Unit =
      start.shiftBy(row, col)
      end.shiftBy(row, col)

  case class Grid(grid: Seq[Seq[Pos]], rows: Int, cols: Int):
    def digTrench(trench: Trench): Unit = trench.dir match
      case U =>
        for row <- trench.end.row to trench.start.row
        do grid(row)(trench.start.col).dig
      case D =>
        for row <- trench.start.row to trench.end.row
        do grid(row)(trench.start.col).dig
      case L =>
        for col <- trench.end.col to trench.start.col
        do grid(trench.start.row)(col).dig
      case R =>
        for col <- trench.start.col to trench.end.col
        do grid(trench.start.row)(col).dig

    def area: Int = grid.map(_.rayCast).sum

object Parsing:
  import DataDefs.*, Dir.*
  private def parseDig(line: String): Dig = line match
    case s"$dir $size (#$rgb)" => Dig(dir.toDir, size.toInt, rgb)

  def parseDigs(lines: List[String]): List[Dig] = lines.map(parseDig(_))

object Solving:
  import DataDefs.*, Dir.*
  private def digOne(start: Pos)(dig: Dig): Trench = dig.dir match
    case U => Trench(U, start, Pos(start.row - dig.size, start.col, '#'))
    case D => Trench(D, start, Pos(start.row + dig.size, start.col, '#'))
    case L => Trench(L, start, Pos(start.row, start.col - dig.size, '#'))
    case R => Trench(R, start, Pos(start.row, start.col + dig.size, '#'))

  @annotation.tailrec
  private def digAll(start: Pos)(digs: List[Dig])(trenches: List[Trench]): List[Trench] =
    if digs.isEmpty then trenches
    else
      val dig = digs.head
      val trench = digOne(start)(dig)
      digAll(trench.end)(digs.tail)(trench :: trenches)

  private def adjustTrenchCoordinates(trenches: List[Trench]): Unit =
    val minCol = trenches.map(_.minCol).min
    val minRow = trenches.map(_.minRow).min
    trenches.foreach(_.shiftBy(minRow, minCol))

  private def makeGrid(trenches: List[Trench]): Grid =
    val rows = trenches.map(_.maxRow).max + 1
    val cols = trenches.map(_.maxCol).max + 1
    val g =
      for row <- 0 until rows
      yield for col <- 0 until cols
      yield Pos(row, col, '.')
    val grid = Grid(g, rows, cols)
    trenches.foreach(grid.digTrench(_))
    grid

  private val start = Pos(0, 0, '#')

  def solve1(lines: List[String]): Int =
    val digs = Parsing.parseDigs(lines)
    val trenches = digAll(start)(digs)(Nil)
    adjustTrenchCoordinates(trenches)
    val grid = makeGrid(trenches)
    grid.area

  def solve2(lines: List[String]): Int = 0

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "18.test.input.txt").toList
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
Testing.result1 // part 1: 62
// Testing.result2 // part 2: 952408144115

object Main:
  private lazy val lines = os.read.lines(os.pwd / "18.input.txt").toList
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
Main.result1 // part 1: 36807
// Main.result2 // part 2: ???
