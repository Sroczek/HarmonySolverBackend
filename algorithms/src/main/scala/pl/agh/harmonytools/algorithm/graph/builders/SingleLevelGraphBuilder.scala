package pl.agh.harmonytools.algorithm.graph.builders

import pl.agh.harmonytools.algorithm.evaluator.{Connection, ConnectionEvaluator}
import pl.agh.harmonytools.algorithm.generator.{GeneratorInput, LayerGenerator}
import pl.agh.harmonytools.algorithm.graph.SingleLevelGraph
import pl.agh.harmonytools.algorithm.graph.node.{
  DoubleLevelLayer,
  Layer,
  NeighbourNode,
  Node,
  NodeContent,
  NodeWithNestedLayer
}

class SingleLevelGraphBuilder[T <: NodeContent, S <: GeneratorInput](first: Node[T], last: Node[T]) {

  def this(firstContent: T, lastContent: T) = {
    this(new Node[T](firstContent), new Node[T](lastContent))
  }

  private var evaluator: Option[ConnectionEvaluator[T]]            = None
  private var generator: Option[LayerGenerator[T, S]]              = None
  private var generatorInputs: Option[List[S]]                     = None
  private var connectedLayers: Option[List[Layer[T]]]              = None
  private var graphTemplate: Option[SingleLevelGraphBuilder[T, S]] = None
  protected var layers: Option[List[Layer[T]]]                     = None

  def withEvaluator(evaluator: ConnectionEvaluator[T]): Unit = this.evaluator = Some(evaluator)

  def withGenerator(generator: LayerGenerator[T, S]): Unit = this.generator = Some(generator)

  def withGeneratorInput(generatorInputs: List[S]): Unit = this.generatorInputs = Some(generatorInputs)

  def withConnectedLayers(connectedLayers: List[Layer[T]]): Unit = this.connectedLayers = Some(connectedLayers)

  def withGraphTemplate(graphTemplate: SingleLevelGraphBuilder[T, S]): Unit = this.graphTemplate = Some(graphTemplate)

  private def withLayers(layers: List[Layer[T]]): Unit = this.layers = Some(layers)

  protected[builders] def getLayers: List[Layer[T]] = layers.getOrElse(sys.error("Connected layers not defined"))

  protected def pushLayer(layer: Layer[T]*): Unit =
    layers match {
      case Some(layerList) => withLayers(layerList ++ layer)
      case None            => sys.error("Connected layers not defined")
    }

  private def getEvaluator: ConnectionEvaluator[T] = evaluator.getOrElse(sys.error("Evaluator not defined"))

  protected def getInputs: List[S] = generatorInputs.getOrElse(sys.error("Inputs not defined"))

  protected def getGenerator: LayerGenerator[T, S] = generator.getOrElse(sys.error("Generator not defined"))

  protected[builders] def getFirst: Node[T] = first

  protected[builders] def getLast: Node[T] = last

  private def getConnectedLayers: List[Layer[T]] =
    connectedLayers.getOrElse(sys.error("ConnectedLayers not defined"))

  private def getGraphTemplate: SingleLevelGraphBuilder[T, S] =
    graphTemplate.getOrElse(sys.error("GraphTemplate not defined"))

  private def removeUnexpectedNeighboursIfExists(): Unit =
    for (layerId <- getLayers.dropRight(1).indices)
      getLayers(layerId).leaveOnlyNodesTo(getLayers(layerId + 1))

  protected def generateLayers(): Unit =
    getInputs.foreach(input => pushLayer(new Layer[T](getGenerator.generate(input).map(new Node[T](_)))))

  private def addEdges(): Unit = {
    for (layerId <- getLayers.dropRight(1).indices)
      getLayers(layerId)
        .connectWith(getLayers(layerId + 1), getEvaluator, layerId == 0, removeUnreachable = true)
  }

  private def addFirstAndLast(): Unit = {
    getLayers.head.getNodeList.foreach { node =>
      first.addNextNeighbour(new NeighbourNode[T](node))
    }

    getLayers.last.getNodeList.foreach { node =>
      node.addNextNeighbour(new NeighbourNode[T](last))
    }
  }

  private def removeUnreachableNodes(): Unit =
    getLayers.last.removeUnreachableNodes()

  private def removeUselessNodes(): Unit =
    for (layer <- getLayers.reverse)
      layer.removeUselessNodes()

  private def makeAllNodesHaveSinglePrevContent(): Unit = {
    for (layerId <- getLayers.reverse.indices) {
      for (currentNode <- getLayers(layerId).getNodeList) {
        if (currentNode.getPrevNeighbours.length > 1) {
          var duplicates: List[Node[T]] = List.empty
          for (prevNeighbour <- currentNode.getPrevNeighbours)
            duplicates = duplicates :+ currentNode.duplicate()
          currentNode.removeLeftConnections()

          val prevNeighbours = currentNode.getPrevNeighbours

          prevNeighbours.head.node.addNextNeighbour(new NeighbourNode[T](currentNode))
          for (i <- 1 to duplicates.length) {
            prevNeighbours(i).node.addNextNeighbour(new NeighbourNode[T](duplicates(i - 1)))
            getLayers(layerId).addNode(duplicates(i - 1))
          }
        }
      }
    }
  }

  private def setEdgeWeights(): Unit = {
    for (layerId <- getLayers.indices) {
      for (currentNode <- getLayers(layerId).getNodeList) {
        val prevNodeContent: Option[T] =
          if (layerId == 0 || getEvaluator.getConnectionSize != 3) None else Some(currentNode.getPrevContentIfSingle)
        for (nextNeighbour <- currentNode.getNextNeighbours) {
          val connection = Connection[T](nextNeighbour.node.getContent, currentNode.getContent, prevNodeContent)
          val w          = getEvaluator.evaluateSoftRules(connection)
          nextNeighbour.setWeight(w)
        }
      }
    }
  }

  def buildWithoutWeights(): SingleLevelGraphBuilder[T, S] = {
    withLayers(List.empty)
    generateLayers()
    addEdges()
    addFirstAndLast()
    removeUselessNodes()
    this
  }

  def build(): SingleLevelGraph[T, S] = {
    val builder: SingleLevelGraphBuilder[T, S] = {
      if (connectedLayers.isDefined) {
        withLayers(getConnectedLayers)
        addFirstAndLast()
        removeUnexpectedNeighboursIfExists()
        removeUnreachableNodes()
        removeUselessNodes()
        this
      } else if (graphTemplate.isDefined)
        getGraphTemplate
      else buildWithoutWeights()
    }

    if (getEvaluator.getConnectionSize == 3)
      builder.makeAllNodesHaveSinglePrevContent()
    builder.setEdgeWeights()

    builder.getResultGraph
  }

  private def getResultGraph: SingleLevelGraph[T, S] =
    new SingleLevelGraph[T, S](getLayers, first, last)

}

class NestedSingleLevelGraphBuilder[T <: NodeContent with GeneratorInput, S <: NodeContent, Q <: GeneratorInput](
  first: NodeWithNestedLayer[T, S],
  last: NodeWithNestedLayer[T, S]
) extends SingleLevelGraphBuilder[T, Q](first, last) {

  def this(firstContent: T, lastContent: T) = {
    this(new NodeWithNestedLayer[T, S](firstContent), new NodeWithNestedLayer[T, S](lastContent))
  }

  override protected def generateLayers(): Unit =
    getInputs.foreach(input =>
      pushLayer(new DoubleLevelLayer[T, S](getGenerator.generate(input).map(new NodeWithNestedLayer[T, S](_))))
    )

  override protected[builders] def getLayers: List[DoubleLevelLayer[T, S]] =
    layers.getOrElse(sys.error("Layers not defined")).asInstanceOf[List[DoubleLevelLayer[T, S]]]

  override protected[builders] def getFirst: NodeWithNestedLayer[T, S] = first

  override protected[builders] def getLast: NodeWithNestedLayer[T, S] = last
}
