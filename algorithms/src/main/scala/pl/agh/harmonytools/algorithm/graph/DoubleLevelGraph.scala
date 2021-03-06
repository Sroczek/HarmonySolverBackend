package pl.agh.harmonytools.algorithm.graph
import pl.agh.harmonytools.algorithm.graph.node.{Layer, NeighbourNode, NeighbourNodes, Node, NodeContent}

class DoubleLevelGraph[T <: NodeContent, S](
  private var doubleLevelLayers: List[Layer[T, S]],
  private val nestedFirst: Node[T, S],
  private val nestedLast: Node[T, S]
) extends ScoreGraph[T, S] {
  final override protected val first: Node[T, S] = nestedFirst
  final override protected val last: Node[T, S]  = nestedLast

  final override def getNodes: List[Node[T, S]] =
    doubleLevelLayers
      .map(_.getNodeList.map(_.getNestedLayer.getNodeList).reduce(_ ++ _))
      .reduce(_ ++ _)
      .concat(List(nestedFirst, nestedLast))

  final def reduceToSingleLevelGraph(): SingleLevelGraph[T, S] = {
    if (getLast.getDistanceFromBeginning == Int.MaxValue)
      throw new InternalError("Shortest paths are not calculated properly: " + getNodes.length)

    var layers: List[Layer[T, S]] = List.empty
    var stack: List[Node[T, S]]   = List(getLast)

    while (stack.length != 1 || stack.head == getFirst) {
      var edges: List[(Node[T, S], Node[T, S])] = List.empty
      var newStack: List[Node[T, S]]            = List.empty
      for (currentNode <- stack) {
        for (prevNode <- currentNode.getPrevsInShortestPath) {
          edges = edges :+ (prevNode, currentNode)
          if (!newStack.contains(prevNode)) newStack = newStack :+ prevNode
        }
      }
      stack.foreach(_.overridePrevNeighbours(NeighbourNodes.empty))
      newStack.foreach(_.overrideNextNeighbours(NeighbourNodes.empty))
      edges.foreach(e => e._1.addNextNeighbour(new NeighbourNode(e._2)))
      stack = newStack
      val layer = new Layer[T, S](stack)
      layers = layers :+ layer
    }
    layers = layers.drop(1)
    getFirst.getNextNeighbours.foreach(_.setWeight(0))
    getLast.getPrevNeighbours.foreach(_.setWeight(0))

    new SingleLevelGraph[T, S](layers, getFirst, getLast)
  }

  def printInfoSingleNode(node: Node[T, S], neighbourNode: NeighbourNode[T, S], layerId: Int): Unit =
    println(Seq(node.getId, neighbourNode.node.getId, layerId + 1, neighbourNode.weight).mkString(","))

  final override def printEdges(): Unit = {
    for (layerId <- doubleLevelLayers.indices) {
      for (layerNode <- doubleLevelLayers(layerId).getNodeList) {
        for (currentNode <- layerNode.getNestedLayer.getNodeList)
          for (neighbour <- currentNode.getNextNeighbours)
            printInfoSingleNode(currentNode, neighbour, layerId + 1)
      }
    }

    val currentNode = nestedFirst
    for (neighbour <- currentNode.getNextNeighbours)
      printInfoSingleNode(currentNode, neighbour, 0)
  }
}
