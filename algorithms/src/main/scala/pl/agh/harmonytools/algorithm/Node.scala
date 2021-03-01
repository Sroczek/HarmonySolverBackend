package pl.agh.harmonytools.algorithm


case class NeighbourNodes[T] (private var nodeList: List[NeighbourNode[T]]) {
  def add(node: NeighbourNode[T]): Unit = {
    nodeList = nodeList.appended(node)
  }

  def size: Int = nodeList.length

  def nonEmpty: Boolean = size > 0

  def getList: List[NeighbourNode[T]] = nodeList

  def remove(node: Node[T]): Unit = {
    nodeList = nodeList.filter(_.node != node)
  }
}

  object Nodes {
    def empty[T]: NeighbourNodes[T] = NeighbourNodes[T](List.empty)
  }

class Node[T](
  private val content: T,
  private var nextNeighbours: NeighbourNodes[T] = Nodes.empty,
  private var prevNeighbours: NeighbourNodes[T] = Nodes.empty
) {

  def getPrevContentIfSingle: NeighbourNode[T] = {
    getUniquePrevContents.headOption.getOrElse(throw new InternalError("Method not allowed in current state of node - there are "
      + getUniquePrevContents.length + " unique prev nodes contents instead of expected 1"))
  }

  def getUniquePrevContents: List[NeighbourNode[T]] = prevNeighbours.getList.distinct

  def getUniquePrevContentsCount: Int = getUniquePrevContents.length

  def haveNext: Boolean = nextNeighbours.getList.nonEmpty

  def havePrev: Boolean = prevNeighbours.getList.nonEmpty

  def addNextNeighbour(neighbourNode: NeighbourNode[T]): Unit = {
    nextNeighbours.add(neighbourNode)
    neighbourNode.node.prevNeighbours.add(new NeighbourNode(this))
  }

  def removeLeftConnections(): Unit = {
    val prevNodes = prevNeighbours.copy()
    for(prevNode <- prevNodes.getList){
      prevNode.node.removeNextNeighbour(this)
    }
  }

  def removeRightConnections(): Unit = {
    while(nextNeighbours.nonEmpty){
      removeNextNeighbour(nextNeighbours.getList.head.node)
    }
  }

  def removeConnections(): Unit = {
    removeLeftConnections()
    removeRightConnections()
  }

  //removes given node from neighbourList in this and this from prevNodes in given node
  def removeNextNeighbour(node: Node[T]) {
    nextNeighbours = NeighbourNodes(nextNeighbours.getList.filter(neighbour => neighbour.node != node))
    node.prevNeighbours.remove(this)
  }

  def overridePrevNodes(newPrevNeighbours: NeighbourNodes[T]) {
    prevNeighbours = newPrevNeighbours;
  }

  def overrideNextNeighbours(newNextNeighbours: NeighbourNodes[T]){
    nextNeighbours = newNextNeighbours;
  }

  def duplicate(): Node[T] =  {
    val newNode = new Node(content);
    for(neighbour <- nextNeighbours.getList){
      newNode.addNextNeighbour(new NeighbourNode(neighbour.node, neighbour.weight))
    }
    newNode;
  }
}
