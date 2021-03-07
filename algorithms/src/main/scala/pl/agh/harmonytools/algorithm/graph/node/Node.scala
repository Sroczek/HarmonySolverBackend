package pl.agh.harmonytools.algorithm.graph.node

import pl.agh.harmonytools.algorithm.graph.dijkstra.DijkstraNode

class Node[T <: NodeContent](
  private val content: T,
  private var nextNeighbours: NeighbourNodes[T] = NeighbourNodes.empty[T],
  private var prevNeighbours: NeighbourNodes[T] = NeighbourNodes.empty[T]
) extends DijkstraNode {

  private var nestedLayer: Option[Layer[T]] = None
//  def setNestedLayer(l: Layer[T, S]): Unit     = nestedLayer = Some(l)
//  def getNestedLayer: Layer[T, S]              = nestedLayer.getOrElse(sys.error("Nested layer not defined"))
//  def hasNestedLayer: Boolean                  = nestedLayer.isDefined

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

  def getUniquePrevContents: List[NeighbourNode[T]] = prevNeighbours.getList.distinct

  def getUniquePrevContentsCount: Int = getUniquePrevContents.length

  def getContent: T = content

  def getPrevNeighbours: List[NeighbourNode[T]] = prevNeighbours.getList

  def getNextNeighbours: List[NeighbourNode[T]] = nextNeighbours.getList

  def hasNext: Boolean = nextNeighbours.nonEmpty

  def hasPrev: Boolean = prevNeighbours.nonEmpty

  def addNextNeighbour(neighbourNode: NeighbourNode[T]): Unit = {
    nextNeighbours.add(neighbourNode)
    neighbourNode.node.prevNeighbours.add(this)
  }

  def setNextNeighbours(neighboursList: List[NeighbourNode[T]]): Unit =
    nextNeighbours = NeighbourNodes(neighboursList)

  def removeLeftConnections(): Unit = {
    val prevNodes = prevNeighbours.copy()
    for (prevNode <- prevNodes.getList)
      prevNode.node.removeNextNeighbour(this)
  }

  def removeRightConnections(): Unit =
    while (nextNeighbours.nonEmpty)
      removeNextNeighbour(nextNeighbours.getList.head.node)

  def removeConnections(): Unit = {
    removeLeftConnections()
    removeRightConnections()
  }

  /**
   * Removes given node from nextNeighbours in this and this from prevNeighbours in given node.
   * @param node which has to be removed
   */
  def removeNextNeighbour(node: Node[T]): Unit = {
    nextNeighbours = NeighbourNodes(nextNeighbours.getList.filter(neighbour => neighbour.node != node))
    node.prevNeighbours.remove(this)
  }

  def overridePrevNeighbours(newPrevNeighbours: NeighbourNodes[T]): Unit =
    prevNeighbours = newPrevNeighbours;

  def overrideNextNeighbours(newNextNeighbours: NeighbourNodes[T]): Unit =
    nextNeighbours = newNextNeighbours;

  def duplicate(): Node[T] = {
    val newNode = new Node[T](content);
    for (neighbour <- nextNeighbours.getList)
      newNode.addNextNeighbour(new NeighbourNode(neighbour.node, neighbour.weight))
    newNode
  }

  private var distanceFromBeginning: Int            = Int.MaxValue
  private var prevsInShortestPath: List[Node[T]] = List.empty

  override def getDistanceFromBeginning: Int = distanceFromBeginning

  override def getPrevsInShortestPath: List[Node[T]] = prevsInShortestPath

  def setDistanceFromBeginning(distance: Int): Unit         = distanceFromBeginning = distance
  def setPrevsInShortestPath(prevs: List[Node[T]]): Unit = prevsInShortestPath = prevs
  def addPrevsInShortestPath(prevs: Node[T]*): Unit      = prevsInShortestPath ++= prevs

  private var id: Option[Int]       = None
  final def setId(newId: Int): Unit = id = Some(newId)
  final def getId: Option[Int]      = id
}
