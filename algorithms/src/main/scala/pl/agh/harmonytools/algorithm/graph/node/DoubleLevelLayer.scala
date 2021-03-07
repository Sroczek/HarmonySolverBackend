package pl.agh.harmonytools.algorithm.graph.node

class DoubleLevelLayer[T <: NodeContent, S <: NodeContent](private var nodeList: List[NodeWithNestedLayer[T, S]])
  extends Layer[T](nodeList) {
  override def getNodeList: List[NodeWithNestedLayer[T, S]] = nodeList
}
