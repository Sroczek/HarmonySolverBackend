package pl.agh.harmonytools.algorithm.graph.dijkstra

import org.scalatest.{FunSuite, Matchers}
import pl.agh.harmonytools.algorithm.graph.SingleLevelGraph
import pl.agh.harmonytools.algorithm.graph.node.{Layer, NeighbourNode, Node, NodeContent}

class DijkstraAlgorithmTest extends FunSuite with Matchers {

  test("Dijkstra with only one shortest path") {
    case class Content(value: String) extends NodeContent {
      override def isRelatedTo(other: NodeContent): Boolean = ???
    }
    type X = Any
    implicit def String2Content(s: String): Content = Content(s)

    val A = new Node[Content, X]("A")
    val B = new Node[Content, X]("B")
    val C = new Node[Content, X]("C")
    val D = new Node[Content, X]("D")
    val E = new Node[Content, X]("E")
    val F = new Node[Content, X]("F")
    val G = new Node[Content, X]("G")
    val H = new Node[Content, X]("H")

    val first = new Node[Content, X]("first")
    val last  = new Node[Content, X]("last")

    first.setNextNeighbours(
      List(
        new NeighbourNode[Content, X](A, 0),
        new NeighbourNode[Content, X](B, 0),
        new NeighbourNode[Content, X](C, 0)
      )
    )

    A.setNextNeighbours(List(new NeighbourNode[Content, X](D, 10)))
    B.setNextNeighbours(List(new NeighbourNode[Content, X](D, 10), new NeighbourNode[Content, X](E, 10)))
    C.setNextNeighbours(List(new NeighbourNode[Content, X](D, 10), new NeighbourNode[Content, X](E, 0)))
    D.setNextNeighbours(
      List(
        new NeighbourNode[Content, X](F, 10),
        new NeighbourNode[Content, X](G, 10),
        new NeighbourNode[Content, X](H, 10)
      )
    )
    E.setNextNeighbours(
      List(
        new NeighbourNode[Content, X](F, 0),
        new NeighbourNode[Content, X](G, 10),
        new NeighbourNode[Content, X](H, 10)
      )
    )
    F.setNextNeighbours(List(new NeighbourNode[Content, X](last, 0)))
    G.setNextNeighbours(List(new NeighbourNode[Content, X](last, 0)))
    H.setNextNeighbours(List(new NeighbourNode[Content, X](last, 0)))

    val l1     = new Layer[Content, X](List(A, B, C))
    val l2     = new Layer[Content, X](List(D, E))
    val l3     = new Layer[Content, X](List(F, G, H))
    val layers = List(l1, l2, l3)

    val graph = new SingleLevelGraph[Content, X](layers, first, last)

    val dijkstra          = new DijkstraAlgorithm[Content, X](graph)
    val shortestPathNodes = dijkstra.getShortestPathToLastNode
    shortestPathNodes.map(_.getContent) shouldBe List(Content("C"), Content("E"), Content("F"))
  }
}
