package pl.agh.harmonytools.algorithm.graph.dijkstra

import org.scalatest.{FunSuite, Matchers}
import pl.agh.harmonytools.algorithm.graph.SingleLevelGraph
import pl.agh.harmonytools.algorithm.graph.node.{Layer, NeighbourNode, Node, NodeContent}

class DijkstraAlgorithmTest extends FunSuite with Matchers {

  test("Dijkstra with only one shortest path") {
    case class Content(value: String) extends NodeContent {
      override def isRelatedTo(other: NodeContent): Boolean = ???
    }
    implicit def String2Content(s: String): Content = Content(s)

    val A = new Node[Content]("A")
    val B = new Node[Content]("B")
    val C = new Node[Content]("C")
    val D = new Node[Content]("D")
    val E = new Node[Content]("E")
    val F = new Node[Content]("F")
    val G = new Node[Content]("G")
    val H = new Node[Content]("H")

    val first = new Node[Content]("first")
    val last  = new Node[Content]("last")

    first.setNextNeighbours(
      List(
        new NeighbourNode[Content](A, 0),
        new NeighbourNode[Content](B, 0),
        new NeighbourNode[Content](C, 0)
      )
    )

    A.setNextNeighbours(List(new NeighbourNode[Content](D, 10)))
    B.setNextNeighbours(List(new NeighbourNode[Content](D, 10), new NeighbourNode[Content](E, 10)))
    C.setNextNeighbours(List(new NeighbourNode[Content](D, 10), new NeighbourNode[Content](E, 0)))
    D.setNextNeighbours(
      List(
        new NeighbourNode[Content](F, 10),
        new NeighbourNode[Content](G, 10),
        new NeighbourNode[Content](H, 10)
      )
    )
    E.setNextNeighbours(
      List(
        new NeighbourNode[Content](F, 0),
        new NeighbourNode[Content](G, 10),
        new NeighbourNode[Content](H, 10)
      )
    )
    F.setNextNeighbours(List(new NeighbourNode[Content](last, 0)))
    G.setNextNeighbours(List(new NeighbourNode[Content](last, 0)))
    H.setNextNeighbours(List(new NeighbourNode[Content](last, 0)))

    val l1     = new Layer[Content](List(A, B, C))
    val l2     = new Layer[Content](List(D, E))
    val l3     = new Layer[Content](List(F, G, H))
    val layers = List(l1, l2, l3)

    val graph = new SingleLevelGraph[Content, Any](layers, first, last)

    val dijkstra          = new DijkstraAlgorithm[Content](graph)
    val shortestPathNodes = dijkstra.getShortestPathToLastNode
    shortestPathNodes.map(_.getContent) shouldBe List(Content("C"), Content("E"), Content("F"))
  }
}
