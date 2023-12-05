/*
--- Day 5: If You Give A Seed A Fertilizer ---

You take the boat and find the gardener right where you were told he would be:
  managing a giant "garden" that looks more to you like a farm.

"A water source? Island Island is the water source!" You poLong out that Snow
Island isn't receiving any water.

"Oh, we had to stop the water because we ran out of sand to filter it with!
Can't make snow with dirty water. Don't worry, I'm sure we'll get more sand
soon; we only turned off the water a few days... weeks... oh no." His face sinks
Longo a look of horrified realization.

"I've been so busy making sure everyone here has food that I completely forgot
to check why we stopped getting more sand! There's a ferry leaving soon that is
headed over in that direction - it's much faster than your boat. Could you
please go check it out?"

You barely have time to agree to this request when he brings up another. "While
you wait for the ferry, maybe you can help us with our food production problem.
The latest Island Island Almanac just arrived and we're having trouble making
sense of it."

The almanac (your puzzle input) lists all of the seeds that need to be planted.
It also lists what type of soil to use with each kind of seed, what type of
fertilizer to use with each kind of soil, what type of water to use with each
kind of fertilizer, and so on. Every type of seed, soil, fertilizer and so on is
identified with a number, but numbers are reused by each category - that is,
soil 123 and fertilizer 123 aren't necessarily related to each other.

For example:

seeds: 79 14 55 13
seed-to-soil map:
50 98 2
52 50 48
soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15
fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4
water-to-light map:
88 18 7
18 25 70
light-to-temperature map:
45 77 23
81 45 19
68 64 13
temperature-to-humidity map:
0 69 1
1 0 69
humidity-to-location map:
60 56 37
56 93 4

The almanac starts by listing which seeds need to be planted: seeds 79, 14, 55,
and 13.

The rest of the almanac contains a list of maps which describe how to convert
numbers from a source category Longo numbers in a destination category. That is,
the section that starts with seed-to-soil map: describes how to convert a seed
number (the source) to a soil number (the destination). This lets the gardener
and his team know which soil to use with which seeds, which water to use with
which fertilizer, and so on.

Rather than list every source number and its corresponding destination number
one by one, the maps describe entire ranges of numbers that can be converted.
Each line within a map contains three numbers: the destination range start, the
source range start, and the range length.

Consider again the example seed-to-soil map:

50 98 2
52 50 48

The first line has a destination range start of 50, a source range start of 98,
and a range length of 2. This line means that the source range starts at 98 and
contains two values: 98 and 99. The destination range is the same length, but it
starts at 50, so its two values are 50 and 51. With this information, you know
that seed number 98 corresponds to soil number 50 and that seed number 99
corresponds to soil number 51.

The second line means that the source range starts at 50 and contains 48 values:
  50, 51, ..., 96, 97. This corresponds to a destination range starting at 52
  and also containing 48 values: 52, 53, ..., 98, 99. So, seed number 53
  corresponds to soil number 55.

Any source numbers that aren't mapped correspond to the same destination number.
So, seed number 10 corresponds to soil number 10.

So, the entire list of seed numbers and their corresponding soil numbers looks
like this:

seed  soil
0     0
1     1
...   ...
48    48
49    49
50    52
51    53
...   ...
96    98
97    99
98    50
99    51

With this map, you can look up the soil number required for each initial seed
number:
    Seed number 79 corresponds to soil number 81.
    Seed number 14 corresponds to soil number 14.
    Seed number 55 corresponds to soil number 57.
    Seed number 13 corresponds to soil number 13.

The gardener and his team want to get started as soon as possible, so they'd
like to know the closest location that needs a seed. Using these maps, find the lowest location number that corresponds to any of the initial seeds. To do this, you'll need to convert each seed number through other categories until you can find its corresponding location number. In this example, the corresponding types are:

Seed 79, soil 81, fertilizer 81, water 81, light 74, temperature 78,
humidity 78, location 82.
Seed 14, soil 14, fertilizer 53, water 49, light 42, temperature 42,
humidity 43, location 43.
Seed 55, soil 57, fertilizer 57, water 53, light 46, temperature 82,
humidity 82, location 86.
Seed 13, soil 13, fertilizer 52, water 41, light 34, temperature 34,
humidity 35, location 35.

So, the lowest location number in this example is 35.
What is the lowest location number that corresponds to any of the initial seed
numbers?

--- Part Two ---
Everyone will starve if you only plant such a small number of seeds. Re-reading
the almanac, it looks like the seeds: line actually describes ranges of seed
numbers.

The values on the initial seeds: line come in pairs. Within each pair, the first
value is the start of the range and the second value is the length of the range.
So, in the first line of the example above:

seeds: 79 14 55 13

This line describes two ranges of seed numbers to be planted in the garden. The
first range starts with seed number 79 and contains 14 values: 79,80,...,91,92.
The second range starts with seed number 55 and contains 13 values:
55, 56, ..., 66, 67.

Now, rather than considering four seed numbers, you need to consider a total of
27 seed numbers.

In the above example, the lowest location number can be obtained from seed
number 82, which corresponds to soil 84, fertilizer 84, water 84, light 77,
temperature 45, humidity 46, and location 46. So, the lowest location number is
46.

Consider all of the initial seed numbers listed in the ranges on the first line
of the almanac. What is the lowest location number that corresponds to any of
the initial seed numbers?
 */
object DataDefs:
  type Seed = Long
  case class Triple(src: Long, dest: Long, range: Long):
    def contains(other: Long) = src <= other && other < src + range
    def corresponding(other: Long) = dest + (other - src)

object Parsing:
  import DataDefs.*
  def parseSeeds(seeds: String): Array[Seed] = seeds match // part 1
    case s"seeds: $x" => x.split(" ").map(_.toLong)

  def parseSeedRanges(seeds: String): Iterator[Seed] =
    val seedRanges = parseSeeds(seeds).grouped(2).map(seq => (seq(0), seq(1)))
    seedRanges.flatMap((seed, range) => (seed until seed + range))

  def parseTriple(triple: String): Triple = triple match
    case s"$dest $src $range" => Triple(src.toLong, dest.toLong, range.toLong)

  // get lines in the input file between two headings.
  def sliceOf(start: Int, end: Int)(using lines: Seq[String]): Seq[String] =
    lines.drop(start + 1).take(end - start - 1)

  def allTriples(start: Int, end: Int)(using lines: Seq[String]): Seq[Triple] =
    sliceOf(start, end).map(parseTriple)

object Solving:
  import DataDefs.*
  def checkOneTriple(query: Long)(triple: Triple): Option[Long] =
    triple.contains(query) match
      case true  => Some(triple.corresponding(query))
      case false => None

  def checkAllTriples(triples: Seq[Triple])(query: Long): Long =
    triples.find(checkOneTriple(query)(_).isDefined) match
      case None         => query // if no matches in map, query maps to itself.
      case Some(triple) => triple.corresponding(query)

object TestIndices: // line numbers in the input file.
  val ls: Seq[String] = os.read.lines(os.pwd / "05.test.input.txt")
  val seedsIndex = ls.indexWhere(_.startsWith("seeds:"))
  val seedToSoil = ls.indexWhere(_ == "seed-to-soil map:")
  val soilToFert = ls.indexWhere(_ == "soil-to-fertilizer map:")
  val fertToWater = ls.indexWhere(_ == "fertilizer-to-water map:")
  val waterToLight = ls.indexWhere(_ == "water-to-light map:")
  val lightToTemp = ls.indexWhere(_ == "light-to-temperature map:")
  val tempToHumid = ls.indexWhere(_ == "temperature-to-humidity map:")
  val humidToLoc = ls.indexWhere(_ == "humidity-to-location map:")

object MainIndices: // line numbers in the input file.
  val ls: Seq[String] = os.read.lines(os.pwd / "05.input.txt").filter(_.nonEmpty)
  val seedsIndex = ls.indexWhere(_.startsWith("seeds:"))
  val seedToSoil = ls.indexWhere(_ == "seed-to-soil map:")
  val soilToFert = ls.indexWhere(_ == "soil-to-fertilizer map:")
  val fertToWater = ls.indexWhere(_ == "fertilizer-to-water map:")
  val waterToLight = ls.indexWhere(_ == "water-to-light map:")
  val lightToTemp = ls.indexWhere(_ == "light-to-temperature map:")
  val tempToHumid = ls.indexWhere(_ == "temperature-to-humidity map:")
  val humidToLoc = ls.indexWhere(_ == "humidity-to-location map:")

object TestData:
  import TestIndices.*, Parsing.*
  given lines: Seq[String] = ls // avoid repeated passing of lines
  val seeds = parseSeeds(ls(seedsIndex))
  val moreSeeds = parseSeedRanges(ls(seedsIndex))
  val seedToSoilMap = allTriples(seedToSoil, soilToFert)
  val soilToFertMap = allTriples(soilToFert, fertToWater)
  val fertToWaterMap = allTriples(fertToWater, waterToLight)
  val waterToLightMap = allTriples(waterToLight, lightToTemp)
  val lightToTempMap = allTriples(lightToTemp, tempToHumid)
  val tempToHumidMap = allTriples(tempToHumid, humidToLoc)
  val humidToLocMap = allTriples(humidToLoc, lines.size)

object MainData:
  import MainIndices.*, Parsing.*
  given lines: Seq[String] = ls // avoid repeated passing of lines
  val seeds = parseSeeds(ls(seedsIndex)) // part 1
  val moreSeeds = parseSeedRanges(ls(seedsIndex))
  val seedToSoilMap = allTriples(seedToSoil, soilToFert)
  val soilToFertMap = allTriples(soilToFert, fertToWater)
  val fertToWaterMap = allTriples(fertToWater, waterToLight)
  val waterToLightMap = allTriples(waterToLight, lightToTemp)
  val lightToTempMap = allTriples(lightToTemp, tempToHumid)
  val tempToHumidMap = allTriples(tempToHumid, humidToLoc)
  val humidToLocMap = allTriples(humidToLoc, lines.size)

object Testing:
  import TestData.*, Solving.*
  val soils = seeds.map(checkAllTriples(seedToSoilMap)(_)) // part 1
  val ferts = soils.map(checkAllTriples(soilToFertMap)(_))
  val waters = ferts.map(checkAllTriples(fertToWaterMap)(_))
  val lights = waters.map(checkAllTriples(waterToLightMap)(_))
  val temps = lights.map(checkAllTriples(lightToTempMap)(_))
  val humids = temps.map(checkAllTriples(tempToHumidMap)(_))
  val locs = humids.map(checkAllTriples(humidToLocMap)(_))
  val result1 = locs.min // part 1

  val soils2 = moreSeeds.map(checkAllTriples(seedToSoilMap)(_)) // part 2
  val ferts2 = soils2.map(checkAllTriples(soilToFertMap)(_))
  val waters2 = ferts2.map(checkAllTriples(fertToWaterMap)(_))
  val lights2 = waters2.map(checkAllTriples(waterToLightMap)(_))
  val temps2 = lights2.map(checkAllTriples(lightToTempMap)(_))
  val humids2 = temps2.map(checkAllTriples(tempToHumidMap)(_))
  val locs2 = humids2.map(checkAllTriples(humidToLocMap)(_))
  val result2 = locs2.min // part 2
Testing.result1 // part 1: 35
Testing.result2 // part 2: 46

object Main:
  import MainData.*, Solving.*
  val soils = seeds.map(checkAllTriples(seedToSoilMap)(_)) // part 1
  val ferts = soils.map(checkAllTriples(soilToFertMap)(_))
  val waters = ferts.map(checkAllTriples(fertToWaterMap)(_))
  val lights = waters.map(checkAllTriples(waterToLightMap)(_))
  val temps = lights.map(checkAllTriples(lightToTempMap)(_))
  val humids = temps.map(checkAllTriples(tempToHumidMap)(_))
  val locs = humids.map(checkAllTriples(humidToLocMap)(_))
  val result1 = locs.min // part 1

  // These are too slow (brute force), took 40 mins. Needs speeding up!
  // val soils2 = moreSeeds.map(checkAllTriples(seedToSoilMap)(_)) // part 2
  // val ferts2 = soils2.map(checkAllTriples(soilToFertMap)(_))
  // val waters2 = ferts2.map(checkAllTriples(fertToWaterMap)(_))
  // val lights2 = waters2.map(checkAllTriples(waterToLightMap)(_))
  // val temps2 = lights2.map(checkAllTriples(lightToTempMap)(_))
  // val humids2 = temps2.map(checkAllTriples(tempToHumidMap)(_))
  // val locs2 = humids2.map(checkAllTriples(humidToLocMap)(_))
  // val result2 = locs2.min // part 2
Main.result1 // part 1: 318728750
// Main.result2 // part 2: 37384986
