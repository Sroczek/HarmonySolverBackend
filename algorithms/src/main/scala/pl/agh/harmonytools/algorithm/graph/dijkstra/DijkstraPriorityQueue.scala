package pl.agh.harmonytools.algorithm.graph.dijkstra

class DijkstraPriorityQueue[T <: DijkstraNode](implicit ordering: Ordering[T]) {

  private var nodeList: List[T] = List.empty

  def enqueue(node: T): Unit = nodeList = nodeList :+ node

  def dequeue: T = {
    if (isEmpty) throw new InternalError("empty.dequeue")
    val minimum = nodeList.min
    nodeList = nodeList.filterNot(_ == minimum)
    minimum
  }

  def isEmpty: Boolean = nodeList.isEmpty

  def nonEmpty: Boolean = !isEmpty

}
