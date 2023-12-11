/*
--- Day 11: Cosmic Expansion ---
You continue following signs for "Hot Springs" and eventually come across an
observatory. The Elf within turns out to be a researcher studying cosmic
expansion using the giant telescope here.

He doesn't know anything about the missing machine parts; he's only visiting for
    this research project. However, he confirms that the hot springs are the
    next-closest area likely to have people; he'll even take you straight there
    once he's done with today's observation analysis.

Maybe you can help him with the analysis to speed things up?

The researcher has collected a bunch of data and compiled the data into a single
giant image (your puzzle input). The image includes empty space (.) and galaxies
(#). For example:

...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....

The researcher is trying to figure out the sum of the lengths of the shortest
path between every pair of galaxies. However, there's a catch: the universe
expanded in the time it took the light from those galaxies to reach the
observatory.

Due to something involving gravitational effects, only some space expands.
In fact, the result is that any rows or columns that contain no galaxies should
all actually be twice as big.

In the above example, three columns and two rows contain no galaxies:

   v  v  v
 ...#......
 .......#..
 #.........
>..........<
 ......#...
 .#........
 .........#
>..........<
 .......#..
 #...#.....
   ^  ^  ^

These rows and columns need to be twice as big; the result of cosmic expansion
therefore looks like this:

....#........
.........#...
#............
.............
.............
........#....
.#...........
............#
.............
.............
.........#...
#....#.......

Equipped with this expanded universe, the shortest path between every pair of
galaxies can be found. It can help to assign every galaxy a unique number:

....1........
.........2...
3............
.............
.............
........4....
.5...........
............6
.............
.............
.........7...
8....9.......

In these 9 galaxies, there are 36 pairs. Only count each pair once; order within
the pair doesn't matter. For each pair, find any shortest path between the two
galaxies using only steps that move up, down, left, or right exactly one . or #
at a time. (The shortest path between two galaxies is allowed to pass through
another galaxy.)

For example, here is one of the shortest paths between galaxies 5 and 9:

....1........
.........2...
3............
.............
.............
........4....
.5...........
.##.........6
..##.........
...##........
....##...7...
8....9.......

This path has length 9 because it takes a minimum of nine steps to get from
galaxy 5 to galaxy 9 (the eight locations marked # plus the step onto galaxy 9
itself). Here are some other example shortest path lengths:

    Between galaxy 1 and galaxy 7: 15
    Between galaxy 3 and galaxy 6: 17
    Between galaxy 8 and galaxy 9: 5

In this example, after expanding the universe, the sum of the shortest path
between all 36 pairs of galaxies is 374.

Expand the universe, then find the length of the shortest path between every
pair of galaxies. What is the sum of these lengths?

--- Part Two ---
The galaxies are much older (and thus much farther apart) than the researcher
initially estimated.

Now, instead of the expansion you did before, make each empty row or column one
million times larger. That is, each empty row should be replaced with 1000000
empty rows, and each empty column should be replaced with 1000000 empty columns.

(In the example above, if each empty row or column were merely 10 times larger,
the sum of the shortest paths between every pair of galaxies would be 1030.
If each empty row or column were merely 100 times larger, the sum of the
shortest paths between every pair of galaxies would be 8410. However, your
universe will need to expand far beyond these values.)

Starting with the same initial image, expand the universe according to these new
rules, then find the length of the shortest path between every pair of galaxies.
What is the sum of these lengths?
 */
object DataDefs:
  type Galaxy = (Int, Int)
  type Distance = Long
  type Space = Char
  type Universe = Vector[Vector[Space]]
  extension (u: Universe) def show: String = u.map(_.mkString).mkString("\n")

object Parsing:
  import DataDefs.*

  private def findEmptyRows(universe: Universe) =
    (0 until universe.length).filter(row => universe(row).forall(_ == '.'))

  private def findEmptyCols(universe: Universe) =
    for
      col <- 0 until universe(0).length
      column = for row <- 0 until universe.length yield universe(row)(col)
      if column.forall(_ == '.')
    yield col

  def findGalaxies(universe: Universe): Seq[Galaxy] =
    for
      row <- 0 until universe.size
      col <- 0 until universe(0).size
      if universe(row)(col) == '#'
    yield (row, col)

  def expand(universe: Universe)(galaxies: Seq[Galaxy])(factor: Int): Seq[Galaxy] =
    val (emptyRows, emptyCols) = (findEmptyRows(universe), findEmptyCols(universe))
    for
      (row, col) <- galaxies
      rowsBefore = emptyRows.count(_ < row)
      colsBefore = emptyCols.count(_ < col)
    yield (row + rowsBefore * (factor - 1), col + colsBefore * (factor - 1))

  def toUniverse(lines: Seq[String]): Universe = lines.map(_.toVector).toVector

object Solving:
  import DataDefs.*
  private def distance(g1: Galaxy, g2: Galaxy): Distance =
    val (row1, col1) = g1
    val (row2, col2) = g2
    math.abs(row1 - row2) + math.abs(col1 - col2)

  private def allDistances(galaxies: Seq[Galaxy]): Seq[Distance] = galaxies
    .combinations(2)
    .map { case Seq(g1, g2) => distance(g1, g2).toLong }
    .toSeq

  def solve(lines: Seq[String])(factor: Int): Long =
    val universe = Parsing.toUniverse(lines)
    val galaxies = Parsing.findGalaxies(universe)
    val expanded = Parsing.expand(universe)(galaxies)(factor)
    allDistances(expanded).sum

object Testing:
  lazy val testInput =
    """...#......
      |.......#..
      |#.........
      |..........
      |......#...
      |.#........
      |.........#
      |..........
      |.......#..
      |#...#.....""".stripMargin.split("\n").toSeq
  lazy val result1 = Solving.solve(testInput)(2)
  lazy val result2 = Solving.solve(testInput)(10)
Testing.result1 // part 1: 374
Testing.result2 // part 2: 1030

object Main:
  lazy val lines = os.read.lines(os.pwd / "11.input.txt")
  lazy val result1 = Solving.solve(lines)(2)
  lazy val result2 = Solving.solve(lines)(1000000)
Main.result1 // part 1: 9556896
Main.result2 // part 2: 685038186836
