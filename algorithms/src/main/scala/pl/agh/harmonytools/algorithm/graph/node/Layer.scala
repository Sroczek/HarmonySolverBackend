package pl.agh.harmonytools.algorithm.graph.node

import pl.agh.harmonytools.algorithm.evaluator.{Connection, ConnectionEvaluator}

class Layer[T <: NodeContent](private var nodeList: List[Node[T]]) {

  def addNode(node: Node[T]): Unit = nodeList = nodeList :+ node

  def getNodeList: List[Node[T]] = nodeList

  def removeNode(node: Node[T]): Unit = {
    nodeList = nodeList.filter(_ != node)
    node.removeConnections()
  }

  def getPrevConnectionsCount: Int =
    nodeList.map(_.getPrevNeighbours.size).sum

  def getNextConnectionsCount: Int =
    nodeList.map(_.getNextNeighbours.size).sum

  def connectWith(
    other: Layer[T],
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

  def leaveOnlyNodesTo(other: Layer[T]): Unit = {
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

  def map(f: Node[T] => Node[T]): Unit =
    nodeList = nodeList.map(f(_))

  def isEmpty: Boolean =
    nodeList.isEmpty
}
