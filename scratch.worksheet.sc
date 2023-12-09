import scalax.collection.OneOrMore
import scalax.collection.mutable.Graph
import scalax.collection.edges.DiEdge
import scalax.collection.edges.DiEdgeImplicits
import scalax.collection.edges.multilabeled.LDiEdge

enum Move:
  case Left, Right
import Move.*

type Node = String
case class Bond(label: Node, left: Node, right: Node)

extension (e: DiEdge[Node])
  def +:(move: Move) = new LDiEdge[Node, Move]:
    def label: Move = move
    def source: Node = e.source
    def target: Node = e.target

def populate(bonds: Seq[Bond]): Graph[Node, LDiEdge[Node, Move]] =
  val wasteland: Graph[Node, LDiEdge[Node, Move]] = Graph.empty
  bonds.foreach(bond =>
    wasteland += bond.label
    wasteland += bond.left
    wasteland += bond.right
    // wasteland += bond.label ~> bond.left // +: Left
    // wasteland += bond.label ~> bond.right // +: Right
  )
  wasteland
