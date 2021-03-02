package pl.agh.harmonytools.algorithm.graph.node

class NeighbourNode[T](val node: Node[T], var weight: Int = 0) {
  def setWeight(weight: Int): Unit =
    this.weight = weight;
}
