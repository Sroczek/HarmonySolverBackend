package pl.agh.harmonytools.algorithm.graph.dijkstra

import org.scalatest.{FunSuite, Matchers}
import pl.agh.harmonytools.algorithm.graph.SingleLevelGraph
import pl.agh.harmonytools.algorithm.graph.node.{Layer, NeighbourNode, Node}

class DijkstraAlgorithmTest extends FunSuite with Matchers {
  test("Dijkstra with only one shortest path") {
    val A = new Node[String]("A")
    val B = new Node[String]("B")
    val C = new Node[String]("C")
    val D = new Node[String]("D")
    val E = new Node[String]("E")
    val F = new Node[String]("F")
    val G = new Node[String]("G")
    val H = new Node[String]("H")

    val first = new Node[String]("first")
    val last  = new Node[String]("last")

    first.setNextNeighbours(
      List(
        new NeighbourNode[String](A, 0),
        new NeighbourNode[String](B, 0),
        new NeighbourNode[String](C, 0)
      )
    )

    A.setNextNeighbours(List(new NeighbourNode[String](D, 10)))
    B.setNextNeighbours(List(new NeighbourNode[String](D, 10), new NeighbourNode[String](E, 10)))
    C.setNextNeighbours(List(new NeighbourNode[String](D, 10), new NeighbourNode[String](E, 0)))
    D.setNextNeighbours(
      List(
        new NeighbourNode[String](F, 10),
        new NeighbourNode[String](G, 10),
        new NeighbourNode[String](H, 10)
      )
    )
    E.setNextNeighbours(
      List(
        new NeighbourNode[String](F, 0),
        new NeighbourNode[String](G, 10),
        new NeighbourNode[String](H, 10)
      )
    )
    F.setNextNeighbours(List(new NeighbourNode[String](last, 0)))
    G.setNextNeighbours(List(new NeighbourNode[String](last, 0)))
    H.setNextNeighbours(List(new NeighbourNode[String](last, 0)))

    val l1     = new Layer[String, Any](List(A, B, C))
    val l2     = new Layer[String, Any](List(D, E))
    val l3     = new Layer[String, Any](List(F, G, H))
    val layers = List(l1, l2, l3)

    val graph = new SingleLevelGraph[String, Any](layers, first, last)

    val dijkstra          = new DijkstraAlgorithm[String](graph)
    val shortestPathNodes = dijkstra.getShortestPathToLastNode
    shortestPathNodes.map(_.getContent) shouldBe List("C", "E", "F")
  }
}
