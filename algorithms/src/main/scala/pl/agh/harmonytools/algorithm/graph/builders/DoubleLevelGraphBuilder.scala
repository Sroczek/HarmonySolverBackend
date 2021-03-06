package pl.agh.harmonytools.algorithm.graph.builders

import pl.agh.harmonytools.algorithm.evaluator.{Connection, ConnectionEvaluator}
import pl.agh.harmonytools.algorithm.generator.LayerGenerator
import pl.agh.harmonytools.algorithm.graph.DoubleLevelGraph
import pl.agh.harmonytools.algorithm.graph.node.{Layer, NeighbourNode, Node, NodeContent}

abstract class DoubleLevelGraphBuilder[T <: NodeContent, S, Q](firstContent: T, lastContent: T) {
  def createNestedLayer(node: Node[T, S], outerGeneratorInput: S, layerId: Int): Layer[T, S]
  // = new ChordGeneratorInput(node.getContent.harmonicFunction, layerId !=0, outerGeneratorInput.sopranoNote)

  private var outerEvaluator: Option[ConnectionEvaluator[T]] = None
  private var innerEvaluator: Option[ConnectionEvaluator[T]] = None
  private var outerGenerator: Option[LayerGenerator[T, S]]   = None
  private var innerGenerator: Option[LayerGenerator[S, Q]]   = None
  private var outerGeneratorInputs: Option[List[S]]          = None
  private val nestedFirst: Node[T, S]                        = new Node[T, S](firstContent)
  private val nestedLast: Node[T, S]                         = new Node[T, S](lastContent)

  def withOuterEvaluator(outerEvaluator: ConnectionEvaluator[T]): Unit = this.outerEvaluator = Some(outerEvaluator)
  def withInnerEvaluator(innerEvaluator: ConnectionEvaluator[T]): Unit = this.innerEvaluator = Some(innerEvaluator)
  def withOuterGenerator(outerGenerator: LayerGenerator[T, S]): Unit   = this.outerGenerator = Some(outerGenerator)
  def withInnerGenerator(innerGenerator: LayerGenerator[S, Q]): Unit   = this.innerGenerator = Some(innerGenerator)
  def withOuterGeneratorInputs(outerGeneratorInputs: List[S]): Unit =
    this.outerGeneratorInputs = Some(outerGeneratorInputs)

  private def getOuterEvaluator: ConnectionEvaluator[T] =
    outerEvaluator.getOrElse(sys.error("OuterEvaluator not defined"))
  private def getOuterGenerator: LayerGenerator[T, S] =
    outerGenerator.getOrElse(sys.error("OuterEvaluator not defined"))
  private def getOuterGeneratorInputs: List[S] =
    outerGeneratorInputs.getOrElse(sys.error("OuterGeneratorInputs not defined"))
  private def getInnerEvaluator: ConnectionEvaluator[T] =
    innerEvaluator.getOrElse(sys.error("InnerGenerator not defined"))

  private val templateSingleGraphBuilder = new SingleLevelGraphBuilder[T, S](firstContent, lastContent)
  templateSingleGraphBuilder.withEvaluator(getOuterEvaluator)
  templateSingleGraphBuilder.withGenerator(getOuterGenerator)
  templateSingleGraphBuilder.withGeneratorInput(getOuterGeneratorInputs)
  templateSingleGraphBuilder.buildWithoutWeights()

  private def generateNestedLayers(): Unit = {
    for (layerId <- templateSingleGraphBuilder.getLayers.indices) {
      for (currentNode <- templateSingleGraphBuilder.getLayers(layerId).getNodeList) {
        val nestedLayer = createNestedLayer(currentNode, getOuterGeneratorInputs(layerId), layerId)
        currentNode.setNestedLayer(nestedLayer)
      }
    }
  }

  private def connectNestedLayers(): Unit = {
    for (layerId <- 0 until templateSingleGraphBuilder.getLayers.length - 1) {
      for (currentNode <- templateSingleGraphBuilder.getLayers(layerId).getNodeList) {
        for (neighbour <- currentNode.getNextNeighbours)
          currentNode.getNestedLayer.connectWith(
            neighbour.node.getNestedLayer,
            getInnerEvaluator,
            layerId == 0,
            removeUnreachable = false
          )
      }
    }
  }

  private def removeUselessNodesInNestedLayers(): Unit = {
    for (layer <- templateSingleGraphBuilder.getLayers.reverse.tail) {
      for (currentNode <- layer.getNodeList)
        currentNode.getNestedLayer.removeUselessNodes()
    }
  }

  private def removeUnreachableNodesInNestedLayers(): Unit = {
    for (layer <- templateSingleGraphBuilder.getLayers.tail) {
      for (currentNode <- layer.getNodeList)
        currentNode.getNestedLayer.removeUnreachableNodes()
    }
  }

  private def removeNodesWithEmptyNestedLayers(): Unit = {
    for (layer <- templateSingleGraphBuilder.getLayers) {
      for (currentNode <- layer.getNodeList) {
        if (!currentNode.hasNestedLayer)
          layer.removeNode(currentNode)
      }
    }
  }

  private def removeUselessNodes(): Unit = {
    for (layer <- templateSingleGraphBuilder.getLayers.reverse)
      layer.removeUselessNodes()
  }

  private def propagateEdgeWeightIntoNestedLayer(node: Node[T, S], weight: Int, nextNodeContent: T): Unit = {
    for (nestedNode <- node.getNestedLayer.getNodeList) {
      for (nestedNeighbour <- nestedNode.getNextNeighbours) {
        if (nestedNeighbour.node.getContent.isRelatedTo(nextNodeContent))
          nestedNeighbour.setWeight(weight)
      }
    }
  }

  private def setEdgeWeightsAndPropagate(): Unit = {
    for (layer <- templateSingleGraphBuilder.getLayers) {
      for (currentNode <- layer.getNodeList) {
        for (neighbour <- currentNode.getNextNeighbours) {
          val connection = Connection(neighbour.node.getContent, currentNode.getContent)
          val weight     = getOuterEvaluator.evaluateSoftRules(connection)
          neighbour.setWeight(weight)
          propagateEdgeWeightIntoNestedLayer(currentNode, weight, neighbour.node.getContent)
        }
      }
    }
  }

  private def attachNestedFirstAndLast(): Unit = {
    for (currentNode <- templateSingleGraphBuilder.getLayers.head.getNodeList) {
      for (currentNestedNode <- currentNode.getNestedLayer.getNodeList) {
        if (currentNestedNode.hasNext)
          nestedFirst.addNextNeighbour(new NeighbourNode[T, S](currentNestedNode))
      }
    }

    for (currentNode <- templateSingleGraphBuilder.getLayers.last.getNodeList) {
      for (currentNestedNode <- currentNode.getNestedLayer.getNodeList) {
        if (currentNestedNode.hasPrev)
          currentNestedNode.addNextNeighbour(new NeighbourNode[T, S](nestedLast))
      }
    }
  }

  def build(): DoubleLevelGraph[T, S] = {
    generateNestedLayers()
    connectNestedLayers()
    removeUselessNodesInNestedLayers()
    removeUnreachableNodesInNestedLayers()
    setEdgeWeightsAndPropagate()

    removeNodesWithEmptyNestedLayers()

    attachNestedFirstAndLast()

    val result = getResult
    if (result.getNodes.length == 2)
      throw InvalidGraphConstruction("Cannot find any harmonic function sequence which could be harmonised")
    result
  }

  private def getResult: DoubleLevelGraph[T, S] =
    new DoubleLevelGraph[T, S](
      templateSingleGraphBuilder.getLayers,
      templateSingleGraphBuilder.getFirst,
      templateSingleGraphBuilder.getLast
    )
}

case class InvalidGraphConstruction(msg: String) extends InternalError(msg)
