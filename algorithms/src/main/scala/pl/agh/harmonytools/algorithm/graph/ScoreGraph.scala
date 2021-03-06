package pl.agh.harmonytools.algorithm.graph

import pl.agh.harmonytools.algorithm.graph.node.{Node, NodeContent}

import scala.annotation.tailrec

trait ScoreGraph[T <: NodeContent, S] {
  protected val first: Node[T, S]
  protected val last: Node[T, S]

  final def getFirst: Node[T, S] = first

  final def getLast: Node[T, S] = last

  def getNodes: List[Node[T, S]]

  def printEdges(): Unit

  final def enumerateNodes(): Unit = {
    getFirst.setId(-1)
    getLast.setId(-2)
    var currentId = 0

    @tailrec
    def enumerateGivenNodes(nodes: List[Node[T, S]]): Unit = {
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
