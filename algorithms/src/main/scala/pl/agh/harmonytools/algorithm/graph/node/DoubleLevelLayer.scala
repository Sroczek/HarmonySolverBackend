package pl.agh.harmonytools.algorithm.graph.node

class DoubleLevelLayer[T, S](private val nodeList: List[NodeWithNestedLayer[T, S]]) {
  def getNodeList: List[NodeWithNestedLayer[T, S]] = nodeList
}
