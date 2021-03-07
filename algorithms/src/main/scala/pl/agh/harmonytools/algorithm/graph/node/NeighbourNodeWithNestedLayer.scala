package pl.agh.harmonytools.algorithm.graph.node

class NeighbourNodeWithNestedLayer[T <: NodeContent, S <: NodeContent](
  override val node: NodeWithNestedLayer[T, S],
  weight: Int = 0
) extends NeighbourNode[T](node, weight)
