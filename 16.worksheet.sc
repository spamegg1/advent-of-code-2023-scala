/*
--- Day 16: The Floor Will Be Lava ---
With the beam of light completely focused somewhere, the reindeer leads you
deeper still into the Lava Production Facility. At some point, you realize that
the steel facility walls have been replaced with cave, and the doorways are just
cave, and the floor is cave, and you're pretty sure this is actually just a
giant cave.

Finally, as you approach what must be the heart of the mountain, you see a
bright light in a cavern up ahead. There, you discover that the beam of light
you so carefully focused is emerging from the cavern wall closest to the
facility and pouring all of its energy into a contraption on the opposite side.

Upon closer inspection, the contraption appears to be a flat, two-dimensional
square grid containing empty space (.), mirrors (/ and \), and splitters (|, -).

The contraption is aligned so that most of the beam bounces around the grid, but
each tile on the grid converts some of the beam's light into heat to melt the
rock in the cavern.

You note the layout of the contraption (your puzzle input). For example:
.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....

The beam enters in the top-left corner from the left and heading to the right.
Then, its behavior depends on what it encounters as it moves:
  If the beam encounters empty space (.), it continues in the same direction.
  If the beam encounters a mirror (/ or \), the beam is reflected 90 degrees
    depending on the angle of the mirror. For instance, a rightward-moving beam
    that encounters a / mirror would continue upward in the mirror's column,
    while a rightward-moving beam that encounters a \ mirror would continue
    downward from the mirror's column.
  If the beam encounters the pointy end of a splitter (| or -), the beam passes
    through the splitter as if the splitter were empty space. For instance, a
    rightward-moving beam that encounters a - splitter would continue in the
    same direction.
  If the beam encounters the flat side of a splitter (| or -), the beam is split
    into two beams going in each of the two directions the splitter's pointy
    ends are pointing. For instance, a rightward-moving beam that encounters a |
    splitter would split into two beams: one that continues upward from the
    splitter's column and one that continues downward from the splitter's column.

Beams do not interact with other beams; a tile can have many beams passing
through it at the same time. A tile is energized if that tile has at least one
beam pass through it, reflect in it, or split in it.

In the above example, here is how the beam of light bounces around the
contraption:
>|<<<\....
|v-.\^....
.v...|->>>
.v...v^.|.
.v...v^...
.v...v^..\
.v../2\\..
<->-/vv|..
.|<<<2-|.\
.v//.|.v..

Beams are only shown on empty tiles; arrows indicate the direction of the beams.
If a tile contains beams moving in multiple directions, the number of distinct
directions is shown instead. Here is the same diagram but instead only showing
whether a tile is energized (#) or not (.):
######....
.#...#....
.#...#####
.#...##...
.#...##...
.#...##...
.#..####..
########..
.#######..
.#...#.#..
Ultimately, in this example, 46 tiles become energized.

The light isn't energizing enough tiles to produce lava; to debug the
contraption, you need to start by analyzing the current situation. With the beam
starting in the top-left heading right, how many tiles end up being energized?

--- Part Two ---
As you try to work out what might be wrong, the reindeer tugs on your shirt and
leads you to a nearby control panel. There, a collection of buttons lets you
align the contraption so that the beam enters from any edge tile and heading
away from that edge. (You can choose either of two directions for the beam if it
starts on a corner; for instance, if the beam starts in the bottom-right corner,
it can start heading either left or upward.)

So, the beam could start on any tile in the top row (heading downward), any tile
in the bottom row (heading upward), any tile in the leftmost column (heading
right), or any tile in the rightmost column (heading left). To produce lava, you
need to find the configuration that energizes as many tiles as possible.

In the above example, this can be achieved by starting the beam in the fourth
tile from the left in the top row:

.|<2<\....
|v-v\^....
.v.v.|->>>
.v.v.v^.|.
.v.v.v^...
.v.v.v^..\
.v.v/2\\..
<-2-/vv|..
.|<<<2-|.\
.v//.|.v..

Using this configuration, 51 tiles are energized:

.#####....
.#.#.#....
.#.#.#####
.#.#.##...
.#.#.##...
.#.#.##...
.#.#####..
########..
.#######..
.#...#.#..

Find the initial beam configuration that energizes the largest number of tiles;
how many tiles are energized in that configuration?
 */
object DataDefs:
  type Row = Int
  type Col = Int

  enum Direction:
    case N, S, E, W
  import Direction.*

  case class Tile(row: Row, col: Col, content: Char, var isEnergized: Boolean = false):
    def energize: Unit = isEnergized = true
    def deEnergize: Unit = isEnergized = false

  case class Beam(row: Row, col: Col, direction: Direction):
    def nextBeam(newDirection: Direction): Beam = newDirection match
      case N => Beam(row - 1, col, newDirection)
      case S => Beam(row + 1, col, newDirection)
      case E => Beam(row, col + 1, newDirection)
      case W => Beam(row, col - 1, newDirection)

    def advance(tileContent: Char): Beams =
      (direction, tileContent) match
        case (N, '/')  => List(nextBeam(E))
        case (N, '\\') => List(nextBeam(W))
        case (N, '-')  => List(nextBeam(E), nextBeam(W))
        case (S, '/')  => List(nextBeam(W))
        case (S, '\\') => List(nextBeam(E))
        case (S, '-')  => List(nextBeam(E), nextBeam(W))
        case (E, '/')  => List(nextBeam(N))
        case (E, '\\') => List(nextBeam(S))
        case (E, '|')  => List(nextBeam(N), nextBeam(S))
        case (W, '/')  => List(nextBeam(S))
        case (W, '\\') => List(nextBeam(N))
        case (W, '|')  => List(nextBeam(N), nextBeam(S))
        case _         => List(nextBeam(direction))

  type Beams = List[Beam]

  type Line = Seq[Tile]
  case class Grid(tiles: Seq[Line]):
    lazy val rows = tiles.size
    lazy val cols = tiles(0).size

    private def filterBeams(beams: Beams)(visited: Beams): Beams =
      beams.filter: beam =>
        !visited.contains(beam) &&
          0 <= beam.row && beam.row < tiles.size &&
          0 <= beam.col && beam.col < tiles(0).size

    @annotation.tailrec
    final def advanceBeams(beams: Beams)(visited: Beams): Beams = beams match
      case head :: next =>
        val (row, col) = (head.row, head.col)
        val tile = tiles(row)(col)
        tile.energize
        val newBeams = head.advance(tile.content)
        val newVisited = head :: visited
        val nextBeams = filterBeams(newBeams)(newVisited) ::: next
        advanceBeams(nextBeams)(newVisited)
      case Nil => Nil

    def countEnergized: Int = tiles.map(row => row.count(_.isEnergized)).sum

    def reset: Unit =
      for
        line <- tiles
        tile <- line
      do tile.deEnergize

object Parsing:
  import DataDefs.*
  def parseLine(line: String)(row: Row): Line =
    line.zipWithIndex.map((char, col) => Tile(row, col, char))

  def parseFile(lines: Seq[String]): Grid =
    Grid(lines.zipWithIndex.map((line, row) => parseLine(line)(row)))

object Solving:
  import DataDefs.*, Direction.*

  def countEnergized(grid: Grid)(startingBeams: Beams): Int =
    grid.advanceBeams(startingBeams)(Nil)
    val count = grid.countEnergized
    grid.reset // de-energize all tiles before next run
    count

  val part1Beams = List(Beam(0, 0, E))
  def solve1(lines: Seq[String]): Int =
    val grid = Parsing.parseFile(lines)
    countEnergized(grid)(part1Beams)

  def northBeams(size: Int) = for i <- 0 until size yield Beam(size - 1, i, N)
  def southBeams(size: Int) = for i <- 0 until size yield Beam(0, i, S)
  def eastBeams(size: Int) = for i <- 0 until size yield Beam(i, 0, E)
  def westBeams(size: Int) = for i <- 0 until size yield Beam(i, size - 1, W)

  def solve2(lines: Seq[String]): Int =
    val grid = Parsing.parseFile(lines)
    val part2Beams =
      northBeams(grid.cols) ++ southBeams(grid.cols) ++
        eastBeams(grid.rows) ++ westBeams(grid.rows)
    part2Beams.map(beam => countEnergized(grid)(List(beam))).max

object Testing:
  lazy val lines = os.read.lines(os.pwd / "16.test.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
Testing.result1 // part 1: 46
Testing.result2 // part 2: 51

object Main:
  private lazy val lines = os.read.lines(os.pwd / "16.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
Main.result1 // part 1: 8021
// Main.result2 // part 2: 8216 // very slow! 2 mins, needs speeding up
