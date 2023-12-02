/*
--- Day 2: Cube Conundrum ---
You're launched high into the atmosphere! The apex of your trajectory just
barely reaches the surface of a large island floating in the sky. You gently
land in a fluffy pile of leaves. It's quite cold, but you don't see much snow.
An Elf runs over to greet you.

The Elf explains that you've arrived at Snow Island and apologizes for the lack
of snow. He'll be happy to explain the situation, but it's a bit of a walk, so
you have some time. They don't get many visitors up here; would you like to play
a game in the meantime?

As you walk, the Elf shows you a small bag and some cubes which are either red,
green, or blue. Each time you play this game, he will hide a secret number of
cubes of each color in the bag, and your goal is to figure out information about
the number of cubes.

To get information, once a bag has been loaded with cubes, the Elf will reach
into the bag, grab a handful of random cubes, show them to you, and then put
them back in the bag. He'll do this a few times per game.

You play several games and record the information from each game (your puzzle
input). Each game is listed with its ID number (like the 11 in Game 11: ...)
followed by a semicolon-separated list of subsets of cubes that were revealed
from the bag (like 3 red, 5 green, 4 blue).

For example, the record of a few games might look like this:

Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green

In game 1, three sets of cubes are revealed from the bag (and then put back
again). The first set is 3 blue cubes and 4 red cubes; the second set is 1 red
cube, 2 green cubes, and 6 blue cubes; the third set is only 2 green cubes.

The Elf would first like to know which games would have been possible if the bag
contained only 12 red cubes, 13 green cubes, and 14 blue cubes?

In the example above, games 1, 2, and 5 would have been possible if the bag had
been loaded with that configuration. However, game 3 would have been impossible
because at one point the Elf showed you 20 red cubes at once; similarly, game 4
would also have been impossible because the Elf showed you 15 blue cubes at
once. If you add up the IDs of the games that would have been possible, you get
8.

Determine which games would have been possible if the bag had been loaded with
only 12 red cubes, 13 green cubes, and 14 blue cubes. What is the sum of the IDs
of those games?

--- Part Two ---
The Elf says they've stopped producing snow because they aren't getting any
water! He isn't sure why the water stopped; however, he can show you how to get
to the water source to check it out for yourself. It's just up ahead!

As you continue your walk, the Elf poses a second question: in each game you
played, what is the fewest number of cubes of each color that could have been in
the bag to make the game possible?

Again consider the example games from earlier:

Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green

In game 1, the game could have been played with as few as 4 red, 2 green, and 6
blue cubes. If any color had even one fewer cube, the game would have been
impossible.
Game 2 could have been played with a minimum of 1 red, 3 green, and 4 blue cubes
Game 3 must have been played with at least 20 red, 13 green, and 6 blue cubes.
Game 4 required at least 14 red, 3 green, and 15 blue cubes.
Game 5 needed no fewer than 6 red, 3 green, and 2 blue cubes in the bag.

The power of a set of cubes is equal to the numbers of red, green, and blue
cubes multiplied together. The power of the minimum set of cubes in game 1 is
48. In games 2-5 it was 12, 1560, 630, and 36, respectively. Adding up these
five powers produces the sum 2286.

For each game, find the minimum set of cubes that must have been present. What
is the sum of the power of these sets?
 */

// Let's over-engineer everything for fun!
object DataDefs:
  enum Color:
    case Red, Green, Blue
  import Color.*

  // Don't work with strings, convert them to types. Extension methods yay!
  extension (rawColor: String)
    def toColor: Color = rawColor match
      case "red"   => Red
      case "green" => Green
      case "blue"  => Blue

  type Count = Int // how many there are of a kind of cube
  type GameID = Int
  case class Cubes(color: Color, count: Count) // e.g. Cubes(Red, 3) = "3 red"

  // e.g. "3 blue, 4 red;" = Hand((Red, 4), (Green, 0), (Blue, 3))
  case class Hand(red: Cubes, green: Cubes, blue: Cubes):
    def isLegal(that: Hand): Boolean = // compare to another hand
      red.count <= that.red.count &&
        green.count <= that.green.count &&
        blue.count <= that.blue.count
    val power: Int = red.count * green.count * blue.count // for part 2

  case class Game(id: GameID, hand: Hand) // e.g. "Game 1: ..." = Game(1, ...)

object Parsing:
  import DataDefs.*, Color.*

  // some hands have the colors out of order, or don't have all 3 colors.
  // Add missing colors with 0 count, and in correct order.
  def addMissingColors(cubes: List[Cubes]): Hand =
    val red = cubes.find(_.color == Red).getOrElse(Cubes(Red, 0))
    val green =
      cubes.find(_.color == Green).getOrElse(Cubes(Green, 0))
    val blue =
      cubes.find(_.color == Blue).getOrElse(Cubes(Blue, 0))
    Hand(red, green, blue)

  // parse strings like "3 red" or "4 blue" etc.
  def parseCubes(rawCube: String): Cubes = rawCube match
    case s"$count $color" => Cubes(color.toColor, count.toInt)

  // parse strings like "3 blue, 4 red", add missing colors / counts.
  def parseHand(rawHand: String): Hand =
    val matches = rawHand.split(", ").toList
    val cubes = matches.map(parseCubes(_))
    addMissingColors(cubes)

  // parse strings like "Game 13"
  def parseGameId(rawGame: String): GameID = rawGame match
    case s"Game $id" => id.toInt

object GameDefs:
  import DataDefs.*, Color.*, Parsing.*

  // each game has multiple hands, choose highest RGB counts.
  def bestOfAllHands(hands: List[Hand]): Hand =
    val bestRed = hands.map(_.red.count).max
    val bestGreen = hands.map(_.green.count).max
    val bestBlue = hands.map(_.blue.count).max
    Hand(
      Cubes(Red, bestRed),
      Cubes(Green, bestGreen),
      Cubes(Blue, bestBlue)
    )

  // convert one line with multiple hands to a game with highest RGB values.
  def lineToGame(line: String): Game =
    val rawData = line.split(": ").toList // "Game 23: ..."
    val rawGame = rawData(0) // "Game 23"
    val rawHands = rawData(1).split("; ").toList // "3 blue, 4 red; ..."
    val gameId = parseGameId(rawGame)
    val hands = rawHands.map(parseHand(_))
    val bestHand = bestOfAllHands(hands)
    Game(gameId, bestHand)

object Testing:
  import DataDefs.*, GameDefs.*

  val testGoodLine = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
  val testBadLine =
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"

  val goodGame = lineToGame(testGoodLine)
  val badGame = lineToGame(testBadLine)
  val good = goodGame.hand.isLegal(Main.globalLimit)
  val bad = badGame.hand.isLegal(Main.globalLimit)

Testing.good // true
Testing.bad // false

object Main:
  import DataDefs.*, Color.*
  val globalLimit: Hand = Hand(
    Cubes(Red, 12),
    Cubes(Green, 13),
    Cubes(Green, 14)
  )
  val path: os.Path = os.pwd / "02.input.txt"
  val lines: Seq[String] = os.read.lines(path)

// Part 1
Main.lines
  .map(GameDefs.lineToGame(_))
  .filter(_.hand.isLegal(Main.globalLimit))
  .map(_.id)
  .sum // 2913

// Part 2
Main.lines
  .map(GameDefs.lineToGame(_).hand.power)
  .sum // 55593
