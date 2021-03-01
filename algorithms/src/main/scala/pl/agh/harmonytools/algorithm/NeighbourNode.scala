package pl.agh.harmonytools.algorithm

class NeighbourNode[T](val node: Node[T], var weight: Int = 0) {
    def setWeight(weight: Int): Unit = {
      this.weight = weight;
    }
}
