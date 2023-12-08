import scalax.collection.generic.AbstractDiEdge
/*
--- Day 8: Haunted Wasteland ---
You're still riding a camel across Desert Island when you spot a sandstorm
quickly approaching. When you turn to warn the Elf, she disappears before your
eyes! To be fair, she had just finished warning you about ghosts a few minutes
ago.

One of the camel's pouches is labeled "maps" - sure enough, it's full of
documents (your puzzle input) about how to navigate the desert. At least, you're
pretty sure that's what they are; one of the documents contains a list of left/
right instructions, and the rest of the documents seem to describe some kind of
network of labeled nodes.

It seems like you're meant to use the left/right instructions to navigate the
network. Perhaps if you have the camel follow the same instructions, you can
escape the haunted wasteland!

After examining the maps for a bit, two nodes stick out: AAA and ZZZ. You feel
like AAA is where you are now, and you have to follow the left/right
instructions until you reach ZZZ.

This format defines each node of the network individually. For example:
RL
AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)

Starting with AAA, you need to look up the next element based on the next left/
right instruction in your input. In this example, start with AAA and go right
(R) by choosing the right element of AAA, CCC. Then, L means to choose the left
element of CCC, ZZZ. By following the left/right instructions, you reach ZZZ in
2 steps.

Of course, you might not find ZZZ right away. If you run out of left/right
instructions, repeat the whole sequence of instructions as necessary: RL really
means RLRLRLRLRLRLRLRL... and so on. For example, here is a situation that takes
6 steps to reach ZZZ:

LLR
AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)

Starting at AAA, follow the left/right instructions. How many steps are required
to reach ZZZ?

--- Part Two ---
The sandstorm is upon you and you aren't any closer to escaping the wasteland.
You had the camel follow the instructions, but you've barely left your starting
position. It's going to take significantly more steps to escape!

What if the map isn't for people - what if the map is for ghosts? Are ghosts
even bound by the laws of spacetime? Only one way to find out.

After examining the maps a bit longer, your attention is drawn to a curious
fact: the number of nodes with names ending in A is equal to the number ending
in Z! If you were a ghost, you'd probably just start at every node that ends
with A and follow all of the paths at the same time until they all
simultaneously end up at nodes that end with Z.

For example:
LR
11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)

Here, there are two starting nodes, 11A and 22A (because they both end with A).
As you follow each left/right instruction, use that instruction to
simultaneously navigate away from both nodes you're currently on. Repeat this
process until all of the nodes you're currently on end with Z. (If only some of
the nodes you're on end with Z, they act like any other node and you continue as
normal.) In this example, you would proceed as follows:

    Step 0: You are at 11A and 22A.
    Step 1: You choose all of the left paths, leading you to 11B and 22B.
    Step 2: You choose all of the right paths, leading you to 11Z and 22C.
    Step 3: You choose all of the left paths, leading you to 11B and 22Z.
    Step 4: You choose all of the right paths, leading you to 11Z and 22B.
    Step 5: You choose all of the left paths, leading you to 11B and 22C.
    Step 6: You choose all of the right paths, leading you to 11Z and 22Z.

So, in this example, you end up entirely on nodes that end in Z after 6 steps.

Simultaneously start on every node that ends with A. How many steps does it take
before you're only on nodes that end with Z?
 */
import scalax.collection.*, mutable.Graph, edges.{DiEdge, DiEdgeImplicits}
import edges.labeled.LDiEdge

object DataDefs:
  enum Rule: // edge labels
    case Left, Right
  import Rule.*
  type Node = String // node labels
  case class Bond(label: Node, left: Node, right: Node)
  type Wasteland = Graph[Node, LDiEdge[Node, Rule]]

  // for sugar: ... ~> ... +: ... ; ~> is given by DiEdgeImplicits.
  extension (e: DiEdge[Node])
    def +:(rule: Rule) = new LDiEdge[Node, Rule]:
      def label: Rule = rule
      def source: Node = e.source
      def target: Node = e.target

object Parsing:
  import DataDefs.*, Rule.*
  extension (char: Char)
    def toRule = char match
      case 'L' => Left
      case 'R' => Right

  def parseRules(line: String): Seq[Rule] = line.map(_.toRule)

  def parseBond(line: String): Bond = line match
    case s"$label = ($left, $right)" => Bond(label, left, right)

  def parseBonds(lines: Seq[String]): Seq[Bond] = lines.map(parseBond)

  def parseInput(raw: Seq[String]): (Seq[Rule], Seq[Bond]) =
    val lines = raw.filter(_.nonEmpty)
    val (rules, bonds) = (lines(0), lines.drop(1))
    (parseRules(rules), parseBonds(bonds))

object Wasteland:
  import DataDefs.*, Rule.*
  def populate(bonds: Seq[Bond]): Wasteland =
    val wasteland: Wasteland = Graph.empty
    bonds.foreach(bond =>
      wasteland += bond.label
      wasteland += bond.left
      wasteland += bond.right
      wasteland += bond.label ~> bond.left +: Left
      wasteland += bond.label ~> bond.right +: Right
    )
    wasteland

object Solving:
  ???

object Testing:
  lazy val testInput1 =
    """RL
      |
      |AAA = (BBB, CCC)
      |BBB = (DDD, EEE)
      |CCC = (ZZZ, GGG)
      |DDD = (DDD, DDD)
      |EEE = (EEE, EEE)
      |GGG = (GGG, GGG)
      |ZZZ = (ZZZ, ZZZ)""".stripMargin.split("\n").toSeq
  lazy val testInput2 =
    """LLR
      |
      |AAA = (BBB, BBB)
      |BBB = (AAA, ZZZ)
      |ZZZ = (ZZZ, ZZZ)""".stripMargin.split("\n").toSeq
  lazy val (rules1, bonds1) = Parsing.parseInput(testInput1)
  lazy val (rules2, bonds2) = Parsing.parseInput(testInput2)
  lazy val wasteland1 = Wasteland.populate(bonds1)
  lazy val wasteland2 = Wasteland.populate(bonds2)
  lazy val testResult1 = 0
  lazy val testResult2 = 0
Testing.testResult1 // part 1: ???
Testing.testResult2 // part 2: ???

object Main:
  val lines: Seq[String] = os.read.lines(os.pwd / "08.input.txt")
  lazy val (rules, bonds) = Parsing.parseInput(lines)
  lazy val wasteland = Wasteland.populate(bonds)
  val result1 = 0
  val result2 = 0
// Main.wasteland.nodes
Main.result1 // part 1: 11309
Main.result2 // part 2: 13740108158591

// (TVA,20777,20777)
// (VBA,16043,16043)
// (VPA,18673,18673)
// (AAA,11309,11309)
// (DTA,17621,17621)
// (DVA,13939,13939)
