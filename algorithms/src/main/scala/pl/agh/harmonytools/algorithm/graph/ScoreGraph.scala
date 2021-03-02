package pl.agh.harmonytools.algorithm.graph

import pl.agh.harmonytools.algorithm.graph.node.Node

import scala.annotation.tailrec

trait ScoreGraph[T] {
  protected val first: Node[T]
  protected val last: Node[T]

  final def getFirst: Node[T] = first

  final def getLast: Node[T] = last

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
