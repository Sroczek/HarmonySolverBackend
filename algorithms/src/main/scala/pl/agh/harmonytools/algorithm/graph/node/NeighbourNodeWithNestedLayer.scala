package pl.agh.harmonytools.algorithm.graph.node

class NeighbourNodeWithNestedLayer[T <: NodeContent, S](
  override val node: NodeWithNestedLayer[T, S],
  override var weight: Int = 0
) extends NeighbourNode[T, S](node)
