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
      case "U" | "3" => U
      case "D" | "1" => D
      case "L" | "2" => L
      case "R" | "0" => R

  case class Dig(dir: Dir, size: Long)

  type Row = Long
  type Col = Long
  case class Pos(row: Row, col: Col):
    def digOne(dig: Dig): Pos = dig.dir match
      case U => Pos(row - dig.size, col)
      case D => Pos(row + dig.size, col)
      case L => Pos(row, col - dig.size)
      case R => Pos(row, col + dig.size)

object Parsing:
  import DataDefs.*, Dir.*
  def parseDig1(line: String): Dig = line match
    case s"$dir $size (#$color)" => Dig(dir.toDir, size.toLong)

  def parseDig2(line: String): Dig = line match
    case s"$dir $size (#$color)" =>
      Dig(color.drop(5).toDir, Integer.parseInt(color.take(5), 16)) // hex = base 16

object Solving:
  import DataDefs.*, Dir.*

  @annotation.tailrec
  private def digAll(start: Pos)(digs: List[Dig])(posts: List[Pos]): List[Pos] =
    if digs.isEmpty then posts
    else
      val end = start.digOne(digs.head)
      digAll(end)(digs.tail)(end :: posts)

  private val start = Pos(0, 0) // irrelevant, result is same for any start pos

  private def shoelace(posts: List[Pos]): Long = // shoelace theorem / algorithm
    val shiftedPosts = posts.tail :+ posts.head
    posts
      .zip(shiftedPosts)
      .map:
        case (Pos(x1, y1), Pos(x2, y2)) => (y1 + y2) * (x1 - x2)
      .sum / 2

  private def perimeter(digs: List[Dig]): Long = digs.map(_.size).sum + 1

  // Shoelace uses cartesian coordinates, problem uses integer coordinates.
  // This causes a difference between problem's area and shoelace area.
  // For example, digging from (0,0) to (0,6) gives a Cartesian length of 6,
  // but for the sake of the problem that should count as 7,
  // since both endpoints are included: #######
  // So we can calculate the perimeter, add its half, plus 1 (because off by 1).
  private def solve(parseFun: List[String] => List[Dig])(lines: List[String]): Long =
    val digs = parseFun(lines)
    val posts = digAll(start)(digs)(Nil)
    shoelace(posts) + perimeter(digs) / 2 + 1

  def solve1 = solve(_.map(Parsing.parseDig1(_)))
  def solve2 = solve(_.map(Parsing.parseDig2(_)))

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "18.test.input.txt").toList
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
Testing.result1 // part 1: 62
Testing.result2 // part 2: 952408144115

object Main:
  private lazy val lines = os.read.lines(os.pwd / "18.input.txt").toList
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
Main.result1 // part 1: 36807
Main.result2 // part 2: 48797603984357
