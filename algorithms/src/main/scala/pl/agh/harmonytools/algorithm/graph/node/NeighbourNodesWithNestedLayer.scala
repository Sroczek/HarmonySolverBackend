package pl.agh.harmonytools.algorithm.graph.node

class NeighbourNodesWithNestedLayer[T <: NodeContent, S <: NodeContent](private var nodeList: List[NeighbourNodeWithNestedLayer[T, S]])
  extends NeighbourNodes[T](nodeList) {
  override def getList: List[NeighbourNodeWithNestedLayer[T, S]] = nodeList
}

object NeighbourNodesWithNestedLayer {
  def empty[T <: NodeContent, S <: NodeContent]: NeighbourNodesWithNestedLayer[T, S] = new NeighbourNodesWithNestedLayer[T, S](List.empty)
}
