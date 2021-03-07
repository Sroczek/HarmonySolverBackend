package pl.agh.harmonytools.algorithm.graph.node

case class NeighbourNodes[T <: NodeContent](private var nodeList: List[NeighbourNode[T]]) {
  def add(node: Node[T]): Unit =
    nodeList = nodeList.appended(new NeighbourNode(node))

  def add(neighbourNode: NeighbourNode[T]): Unit =
    nodeList = nodeList.appended(neighbourNode)

  def size: Int = nodeList.length

  def nonEmpty: Boolean = size > 0

  def getList: List[NeighbourNode[T]] = nodeList

  def remove(node: Node[T]): Unit =
    nodeList = nodeList.filter(_.node != node)
}

object NeighbourNodes {
  def empty[T <: NodeContent]: NeighbourNodes[T] = NeighbourNodes[T](List.empty)
}
