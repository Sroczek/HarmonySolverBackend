package pl.agh.harmonytools.algorithm.graph.node

case class NeighbourNodes[T <: NodeContent, S](private var nodeList: List[NeighbourNode[T, S]]) {
  def add(node: Node[T, S]): Unit =
    nodeList = nodeList.appended(new NeighbourNode(node))

  def add(neighbourNode: NeighbourNode[T, S]): Unit =
    nodeList = nodeList.appended(neighbourNode)

  def size: Int = nodeList.length

  def nonEmpty: Boolean = size > 0

  def getList: List[NeighbourNode[T, S]] = nodeList

  def remove(node: Node[T, S]): Unit =
    nodeList = nodeList.filter(_.node != node)
}

object NeighbourNodes {
  def empty[T <: NodeContent, S]: NeighbourNodes[T, S] = NeighbourNodes[T, S](List.empty)
}
