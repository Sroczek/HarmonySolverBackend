package pl.agh.harmonytools.algorithm.graph.node

class NeighbourNode[T <: NodeContent, S](val node: Node[T, S], var weight: Int = 0) {
  def setWeight(weight: Int): Unit =
    this.weight = weight;
}
