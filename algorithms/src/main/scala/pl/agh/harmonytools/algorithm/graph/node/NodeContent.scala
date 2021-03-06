package pl.agh.harmonytools.algorithm.graph.node

trait NodeContent {
  def isRelatedTo(other: NodeContent): Boolean
}
