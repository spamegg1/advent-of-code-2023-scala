/*
--- Day 19: Aplenty ---
The Elves of Gear Island are thankful for your help and send you on your way.
They even have a hang glider that someone stole from Desert Island; since you're
already going that direction, it would help them a lot if you would use it to
get down there and return it to them.

As you reach the bottom of the relentless avalanche of machine parts, you
discover that they're already forming a formidable heap. Don't worry, though - a
group of Elves is already here organizing the parts, and they have a system.

To start, each part is rated in each of four categories:
    x: Extremely cool looking
    m: Musical (it makes a noise when you hit it)
    a: Aerodynamic
    s: Shiny

Then, each part is sent through a series of workflows that will ultimately
accept or reject the part. Each workflow has a name and contains a list of
rules; each rule specifies a condition and where to send the part if the
condition is true. The first rule that matches the part being considered is
applied immediately, and the part moves on to the destination described by the
rule. (The last rule in each workflow has no condition and always applies if
reached.)

Consider the workflow ex{x>10:one,m<20:two,a>30:R,A}. This workflow is named ex
and contains four rules. If workflow ex were considering a specific part, it
would perform the following steps in order:
    Rule "x>10:one": If the part's x is more than 10, send the part to the
    workflow named one.
    Rule "m<20:two": Otherwise, if the part's m is less than 20, send the part
    to the workflow named two.
    Rule "a>30:R": Otherwise, if the part's a is more than 30, the part is
    immediately rejected (R).
    Rule "A": Otherwise, because no other rules matched the part, the part is
    immediately accepted (A).

If a part is sent to another workflow, it immediately switches to the start of
that workflow instead and never returns. If a part is accepted (sent to A) or
rejected (sent to R), the part immediately stops any further processing.

The system works but it's not keeping up with the torrent of weird metal shapes.
The Elves ask if you can help sort a few parts and give you the list of
workflows and some part ratings (your puzzle input). For example:

px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}

The workflows are listed first, followed by a blank line, then the ratings of
the parts the Elves would like you to sort. All parts begin in the workflow
named in. In this example, the five listed parts go through the following
workflows:
    {x=787,m=2655,a=1222,s=2876}: in -> qqz -> qs -> lnx -> A
    {x=1679,m=44,a=2067,s=496}: in -> px -> rfg -> gd -> R
    {x=2036,m=264,a=79,s=2244}: in -> qqz -> hdj -> pv -> A
    {x=2461,m=1339,a=466,s=291}: in -> px -> qkq -> crn -> R
    {x=2127,m=1623,a=2188,s=1013}: in -> px -> rfg -> A

Ultimately, three parts are accepted. Adding up the x, m, a, and s rating for
each of the accepted parts gives 7540 for the part with x=787, 4623 for the
part with x=2036, and 6951 for the part with x=2127. Adding all of the ratings
for all of the accepted parts gives the sum total of 19114.

Sort through all of the parts you've been given; what do you get if you add
together all of the rating numbers for all of the parts that ultimately get
accepted?

--- Part Two ---
Even with your help, the sorting process still isn't fast enough.

One of the Elves comes up with a new plan: rather than sort parts individually
through all of these workflows, maybe you can figure out in advance which
combinations of ratings will be accepted or rejected.

Each of the four ratings (x, m, a, s) can have an integer value ranging from a
minimum of 1 to a maximum of 4000. Of all possible distinct combinations of
ratings, your job is to figure out which ones will be accepted.

In the above example, there are 167409079868000 distinct combinations of ratings
that will be accepted.

Consider only your list of workflows; the list of part ratings that the Elves
wanted you to sort is no longer relevant. How many distinct combinations of
ratings will be accepted by the Elves' workflows?
 */
import collection.mutable.Queue

object DataDefs:
  enum Rating(rating: Int):
    case X(rating: Int) extends Rating(rating)
    case M(rating: Int) extends Rating(rating)
    case A(rating: Int) extends Rating(rating)
    case S(rating: Int) extends Rating(rating)
  import Rating.*

  type Label = String
  enum Flow:
    case Name(label: Label)
    case Accepted
    case Rejected
    def isNamed: Boolean = this match
      case Name(label) => true
      case _           => false

  import Flow.*

  enum Rule:
    case Less(category: Rating, flow: Flow)
    case More(category: Rating, flow: Flow)
    case Unconditional(flow: Flow)

    def opposite: Rule = this match
      case Less(category, flow) =>
        category match
          case X(rating) => More(X(rating - 1), flow)
          case M(rating) => More(M(rating - 1), flow)
          case A(rating) => More(A(rating - 1), flow)
          case S(rating) => More(S(rating - 1), flow)
      case More(category, flow) =>
        category match
          case X(rating) => Less(X(rating + 1), flow)
          case M(rating) => Less(M(rating + 1), flow)
          case A(rating) => Less(A(rating + 1), flow)
          case S(rating) => Less(S(rating + 1), flow)
      case Unconditional(flow) => this
  import Rule.*

  case class Workflow(flow: Flow, rules: List[Rule])

  extension (s: String)
    def toFlow: Flow = s match
      case "A" => Accepted
      case "R" => Rejected
      case _   => Name(s)

    def toRating: Rating = s match
      case s"$category.$rating" =>
        category match
          case "x" => X(rating.toInt)
          case "m" => M(rating.toInt)
          case "a" => A(rating.toInt)
          case "s" => S(rating.toInt)

  case class Part(x: X, m: M, a: A, s: S): // part 1
    lazy val totalRating: Long = x.rating + m.rating + a.rating + s.rating

    private def processRule(rule: Rule): Option[Flow] = rule match
      case Less(category, flow) =>
        category match
          case X(rating) => if x.rating < rating then Some(flow) else None
          case M(rating) => if m.rating < rating then Some(flow) else None
          case A(rating) => if a.rating < rating then Some(flow) else None
          case S(rating) => if s.rating < rating then Some(flow) else None
      case More(category, flow) =>
        category match
          case X(rating) => if x.rating > rating then Some(flow) else None
          case M(rating) => if m.rating > rating then Some(flow) else None
          case A(rating) => if a.rating > rating then Some(flow) else None
          case S(rating) => if s.rating > rating then Some(flow) else None
      case Unconditional(flow) => Some(flow)

    def processRules(rules: List[Rule]): Flow = rules match
      case head :: next => processRule(head).getOrElse(processRules(next))
      case Nil          => Rejected // this case should not happen due to Unconditional

  // Part 2
  extension (r: Range)
    def cap(that: Range): Range =
      val lower = math.max(r.start, that.start)
      val higher = math.min(r.end, that.end)
      Range(lower, higher)

  case class Ranges(xs: Range, ms: Range, as: Range, ss: Range, flow: Flow):
    lazy val product: Long = xs.size.toLong * ms.size * as.size * ss.size
    def isEmpty: Boolean = xs.isEmpty || ms.isEmpty || as.isEmpty || ss.isEmpty

    private def processRule(rule: Rule): Ranges = rule match
      case Less(category, flow) =>
        category match
          case X(rating) => Ranges(xs.cap(Range(1, rating)), ms, as, ss, flow)
          case M(rating) => Ranges(xs, ms.cap(Range(1, rating)), as, ss, flow)
          case A(rating) => Ranges(xs, ms, as.cap(Range(1, rating)), ss, flow)
          case S(rating) => Ranges(xs, ms, as, ss.cap(Range(1, rating)), flow)
      case More(category, flow) =>
        category match
          case X(rating) => Ranges(xs.cap(Range(rating + 1, 4001)), ms, as, ss, flow)
          case M(rating) => Ranges(xs, ms.cap(Range(rating + 1, 4001)), as, ss, flow)
          case A(rating) => Ranges(xs, ms, as.cap(Range(rating + 1, 4001)), ss, flow)
          case S(rating) => Ranges(xs, ms, as, ss.cap(Range(rating + 1, 4001)), flow)
      case Unconditional(flow) => Ranges(xs, ms, as, ss, flow)

    def processRules(rules: List[Rule])(acc: Queue[Ranges]): Queue[Ranges] =
      rules match
        case head :: next =>
          val normal = processRule(head)
          val opposite = processRule(head.opposite)
          acc.enqueue(normal)
          opposite.processRules(next)(acc)
        case Nil => acc

object Parsing:
  import DataDefs.*, Rule.*, Flow.*, Rating.*

  private def parsePart(line: String): Part = line match
    case s"{x=$x,m=$m,a=$a,s=$s}" =>
      Part(X(x.toInt), M(m.toInt), A(a.toInt), S(s.toInt))

  def parseParts(lines: List[String]): List[Part] = lines.map(parsePart(_))

  private def parseRule(line: String): Rule = line match
    case s"$inequality:$flow" =>
      inequality match
        case s"$category<$rating" => Less(s"$category.$rating".toRating, flow.toFlow)
        case s"$category>$rating" => More(s"$category.$rating".toRating, flow.toFlow)

  private def parseRules(rules: List[String]): List[Rule] = rules.map(parseRule(_))

  private def parseWorkflow(line: String): Workflow = line match
    case s"$label{$stuff}" =>
      val parsedStuff = stuff.split(",").toList
      val (listOfRules, flow) = (parsedStuff.init, parsedStuff.last)
      Workflow(label.toFlow, parseRules(listOfRules) :+ Unconditional(flow.toFlow))

  def parseWorkflows(lines: List[String]): Map[Flow, Workflow] = lines
    .map(parseWorkflow(_))
    .map(workflow => workflow.flow -> workflow)
    .toMap

object Solving:
  import DataDefs.*, Rule.*, Flow.*, Rating.*

  // Part 1
  private def processPart(workflows: Map[Flow, Workflow])(part: Part): (Part, Flow) =
    var flow = Name("in")
    while flow.isNamed do
      val workflow = workflows(flow)
      flow = part.processRules(workflow.rules)
    (part, flow)

  private def processParts(workflows: Map[Flow, Workflow])(parts: List[Part]): Long =
    parts
      .map(processPart(workflows))
      .filter(_._2 == Accepted)
      .map(_._1.totalRating)
      .sum

  // Part 2
  private val start =
    Ranges(Range(1, 4001), Range(1, 4001), Range(1, 4001), Range(1, 4001), Name("in"))

  private def processRanges(workflows: Map[Flow, Workflow]): Long =
    var result = 0L
    var ranges = start
    val queue = Queue(start)
    while queue.nonEmpty do
      ranges = queue.dequeue()
      if ranges.flow == Accepted then result += ranges.product
      else if ranges.flow == Rejected || ranges.isEmpty then ()
      else
        val workflow = workflows(ranges.flow)
        queue ++= ranges.processRules(workflow.rules)(Queue[Ranges]())
    result

  def solve1(lines1: List[String])(lines2: List[String]): Long =
    val workflows = Parsing.parseWorkflows(lines1)
    val parts = Parsing.parseParts(lines2)
    processParts(workflows)(parts)

  def solve2(lines: List[String]): Long =
    val workflows = Parsing.parseWorkflows(lines)
    processRanges(workflows)

object Testing:
  private lazy val workflows = os.read.lines(os.pwd / "19.test.input.txt").toList
  private lazy val parts = os.read.lines(os.pwd / "19.test.input.2.txt").toList
  lazy val result1 = Solving.solve1(workflows)(parts)
  lazy val result2 = Solving.solve2(workflows)
Testing.result1 // part 1: 19114
Testing.result2 // part 2: 167.409.079.868.000

object Main:
  private lazy val workflows = os.read.lines(os.pwd / "19.input.txt").toList
  private lazy val parts = os.read.lines(os.pwd / "19.input.2.txt").toList
  lazy val result1 = Solving.solve1(workflows)(parts)
  lazy val result2 = Solving.solve2(workflows)
Main.result1 // part 1: 348378
Main.result2 // part 2: 121.158.073.425.385
