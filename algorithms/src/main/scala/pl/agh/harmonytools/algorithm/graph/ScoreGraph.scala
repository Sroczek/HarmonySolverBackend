package pl.agh.harmonytools.algorithm.graph

import pl.agh.harmonytools.algorithm.graph.dijkstra.DijkstraNode
import pl.agh.harmonytools.algorithm.graph.node.{Node, NodeContent}

import scala.annotation.tailrec

trait ScoreGraph[T <: NodeContent] {
  protected val first: Node[T]
  protected val last: Node[T]

  def getFirst: Node[T] = first

  def getLast: Node[T] = last

  def getNodes: List[Node[T]]

  def printEdges(): Unit

  final def enumerateNodes(): Unit = {
    getFirst.setId(-1)
    getLast.setId(-2)
    var currentId = 0

    @tailrec
    def enumerateGivenNodes(nodes: List[Node[T]]): Unit = {
      nodes match {
        case head :: tail =>
          head.setId(currentId)
          currentId += 1
          enumerateGivenNodes(tail)
        case Nil =>
      }
    }
    enumerateGivenNodes(getNodes)
  }
}
