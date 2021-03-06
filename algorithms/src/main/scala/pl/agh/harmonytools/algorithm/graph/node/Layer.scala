package pl.agh.harmonytools.algorithm.graph.node

import pl.agh.harmonytools.algorithm.evaluator.{Connection, ConnectionEvaluator}
import pl.agh.harmonytools.algorithm.generator.LayerGenerator

class Layer[T <: NodeContent, S](private var nodeList: List[Node[T, S]]) {

  def this(generatorInput: S, generator: LayerGenerator[T, S]) = {
    this(generator.generate(generatorInput).map(new Node[T, S](_)))
  }

  def addNode(node: Node[T, S]): Unit = nodeList = nodeList :+ node

  def getNodeList: List[Node[T, S]] = nodeList

  def removeNode(node: Node[T, S]): Unit = {
    nodeList = nodeList.filter(_ != node)
    node.removeConnections()
  }

  def getPrevConnectionsCount: Int =
    nodeList.map(_.getPrevNeighbours.size).sum

  def getNextConnectionsCount: Int =
    nodeList.map(_.getNextNeighbours.size).sum

  def connectWith(
    other: Layer[T, S],
    evaluator: ConnectionEvaluator[T],
    isFirstLayer: Boolean,
    removeUnreachable: Boolean
  ): Unit = {
    nodeList.foreach { currentNode =>
      if (currentNode.hasPrev || isFirstLayer) {
        other.getNodeList.foreach { nextNode =>
          if (evaluator.evaluateHardRules(Connection(nextNode.getContent, currentNode.getContent)))
            currentNode.addNextNeighbour(new NeighbourNode(nextNode))
        }
      }
    }
    if (removeUnreachable) other.removeUnreachableNodes()
  }

  def leaveOnlyNodesTo(other: Layer[T, S]): Unit = {
    nodeList.foreach { currentNode =>
      currentNode.getNextNeighbours.foreach { currentNeighbour =>
        if (other.getNodeList.contains(currentNeighbour))
          currentNode.removeNextNeighbour(currentNeighbour.node)
      }
    }
  }

  def removeUselessNodes(): Unit = {
    nodeList.foreach { currentNode =>
      if (!currentNode.hasNext) removeNode(currentNode)
    }
  }

  def removeUnreachableNodes(): Unit = {
    nodeList.foreach { currentNode =>
      if (!currentNode.hasPrev) removeNode(currentNode)
    }
  }

  def map(f: Node[T, S] => Node[T, S]): Unit =
    nodeList = nodeList.map(f(_))

  def isEmpty: Boolean =
    nodeList.isEmpty
}
