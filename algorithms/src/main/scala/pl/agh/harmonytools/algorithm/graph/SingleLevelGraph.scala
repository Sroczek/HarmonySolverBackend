package pl.agh.harmonytools.algorithm.graph

import pl.agh.harmonytools.algorithm.graph.node.{Layer, Node, NodeContent}

class SingleLevelGraph[T <: NodeContent, S](
  private val layers: List[Layer[T]],
  private val firstNode: Node[T],
  private val lastNode: Node[T]
) extends ScoreGraph[T] {
  override protected val first: Node[T] = firstNode
  override protected val last: Node[T]  = lastNode

  override def getNodes: List[Node[T]] =
    layers.map(_.getNodeList).reduce(_ ++ _).concat(List(first, last))

  def getLayers: List[Layer[T]] = layers

  def printInfoSingleNode(node: Node[T], layerId: Int): Unit =
    println(Seq(node.getId, node.getContent, layerId).mkString(","))

  final override def printEdges(): Unit = {
    printInfoSingleNode(first, 0)
    for (layerId <- layers.indices)
      for (node <- layers(layerId).getNodeList)
        printInfoSingleNode(node, layerId + 1)
    printInfoSingleNode(last, layers.length + 1)
  }
}
