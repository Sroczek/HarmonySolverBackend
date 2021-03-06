package pl.agh.harmonytools.algorithm.graph.node

case class NeighbourNodesWithNestedLayer[T <: NodeContent, S](private var nodeList: List[NeighbourNodeWithNestedLayer[T, S]])
  extends NeighbourNodes[T, S](nodeList) {
  override def getList: List[NeighbourNodeWithNestedLayer[T, S]] = nodeList
}

object NeighbourNodesWithNestedLayer {
  def empty[T <: NodeContent, S]: NeighbourNodesWithNestedLayer[T, S] = NeighbourNodesWithNestedLayer[T, S](List.empty)
}
