package pl.agh.harmonytools.algorithm.graph.node

import pl.agh.harmonytools.algorithm.graph.dijkstra.DijkstraNode

class Node[T](
  private val content: T,
  private var nextNeighbours: NeighbourNodes[T] = NeighbourNodes.empty[T],
  private var prevNeighbours: NeighbourNodes[T] = NeighbourNodes.empty[T]
) extends DijkstraNode {

  private var id: Option[Int]       = None
  final def setId(newId: Int): Unit = id = Some(newId)
  final def getId: Option[Int]      = id

  final def getPrevContentIfSingle: T =
    getUniquePrevContents.headOption
      .getOrElse(
        throw new InternalError(
          "Method not allowed in current state of node - there are "
            + getUniquePrevContents.length + " unique prev nodes contents instead of expected 1"
        )
      )
      .node
      .getContent

  final def getUniquePrevContents: List[NeighbourNode[T]] = prevNeighbours.getList.distinct

  final def getUniquePrevContentsCount: Int = getUniquePrevContents.length

  final def getContent: T = content

  final def getPrevNeighbours: List[NeighbourNode[T]] = prevNeighbours.getList

  final def getNextNeighbours: List[NeighbourNode[T]] = nextNeighbours.getList

  final def hasNext: Boolean = nextNeighbours.nonEmpty

  final def hasPrev: Boolean = prevNeighbours.nonEmpty

  final def addNextNeighbour(neighbourNode: NeighbourNode[T]): Unit = {
    nextNeighbours.add(neighbourNode)
    neighbourNode.node.prevNeighbours.add(this)
  }

  final def setNextNeighbours(neighboursList: List[NeighbourNode[T]]): Unit =
    nextNeighbours = NeighbourNodes(neighboursList)

  final def removeLeftConnections(): Unit = {
    val prevNodes = prevNeighbours.copy()
    for (prevNode <- prevNodes.getList)
      prevNode.node.removeNextNeighbour(this)
  }

  final def removeRightConnections(): Unit = {
    while (nextNeighbours.nonEmpty)
      removeNextNeighbour(nextNeighbours.getList.head.node)
  }

  final def removeConnections(): Unit = {
    removeLeftConnections()
    removeRightConnections()
  }

  /**
   * Removes given node from nextNeighbours in this and this from prevNeighbours in given node.
   * @param node which has to be removed
   */
  final def removeNextNeighbour(node: Node[T]): Unit = {
    nextNeighbours = NeighbourNodes(nextNeighbours.getList.filter(neighbour => neighbour.node != node))
    node.prevNeighbours.remove(this)
  }

  final def overridePrevNeighbours(newPrevNeighbours: NeighbourNodes[T]): Unit =
    prevNeighbours = newPrevNeighbours;

  final def overrideNextNeighbours(newNextNeighbours: NeighbourNodes[T]): Unit =
    nextNeighbours = newNextNeighbours;

  final def duplicate(): Node[T] = {
    val newNode = new Node[T](content);
    for (neighbour <- nextNeighbours.getList)
      newNode.addNextNeighbour(new NeighbourNode(neighbour.node, neighbour.weight))
    newNode
  }

  private var distanceFromBeginning: Int         = Int.MaxValue
  private var prevsInShortestPath: List[Node[T]] = List.empty

  final override def getDistanceFromBeginning: Int = distanceFromBeginning

  final override def getPrevsInShortestPath: List[Node[T]] = prevsInShortestPath

  final def setDistanceFromBeginning(distance: Int): Unit      = distanceFromBeginning = distance
  final def setPrevsInShortestPath(prevs: List[Node[T]]): Unit = prevsInShortestPath = prevs
  final def addPrevsInShortestPath(prevs: Node[T]*): Unit      = prevsInShortestPath ++= prevs
}
