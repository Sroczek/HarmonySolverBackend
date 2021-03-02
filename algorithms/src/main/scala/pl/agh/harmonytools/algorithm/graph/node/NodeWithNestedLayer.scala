package pl.agh.harmonytools.algorithm.graph.node

class NodeWithNestedLayer[T, S](
  private val content: T,
  private var nextNeighbours: NeighbourNodes[T] = NeighbourNodes.empty,
  private var prevNeighbours: NeighbourNodes[T] = NeighbourNodes.empty,
  private val nestedLayer: Layer[T, S]
) extends Node[T](content, nextNeighbours, prevNeighbours) {
  final def getNestedLayer: Layer[T, S] = nestedLayer
}
