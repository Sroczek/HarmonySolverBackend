package pl.agh.harmonytools.algorithm.graph.dijkstra

trait DijkstraNode {
  def getDistanceFromBeginning: Int
  def getPrevsInShortestPath: List[DijkstraNode]
}
