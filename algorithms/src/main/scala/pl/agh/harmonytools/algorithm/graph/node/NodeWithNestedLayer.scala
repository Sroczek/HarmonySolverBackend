package pl.agh.harmonytools.algorithm.graph.node

class NodeWithNestedLayer[T <: NodeContent, S <: NodeContent](
  private var content: T,
  private var nextNeighbours: NeighbourNodesWithNestedLayer[T, S] = NeighbourNodesWithNestedLayer.empty[T, S],
  private var prevNeighbours: NeighbourNodesWithNestedLayer[T, S] = NeighbourNodesWithNestedLayer.empty[T, S],
  private var nestedLayer: Option[Layer[S]] = None
) extends Node[T](content, nextNeighbours, prevNeighbours) {

  def setNestedLayer(l: Layer[S]): Unit = nestedLayer = Some(l)
  def getNestedLayer: Layer[S]          = nestedLayer.getOrElse(sys.error("Nested layer not defined"))
  def hasNestedLayer: Boolean              = nestedLayer.isDefined

  override def getNextNeighbours: List[NeighbourNodeWithNestedLayer[T, S]] = nextNeighbours.getList

  override def getPrevNeighbours: List[NeighbourNodeWithNestedLayer[T, S]] = prevNeighbours.getList
}
