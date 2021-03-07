package pl.agh.harmonytools.algorithm.graph.dijkstra

trait DijkstraNode {
  def getDistanceFromBeginning: Int
  def getPrevsInShortestPath: List[DijkstraNode]
  private var id: Option[Int]       = None
  final def setId(newId: Int): Unit = id = Some(newId)
  final def getId: Option[Int]      = id
}
