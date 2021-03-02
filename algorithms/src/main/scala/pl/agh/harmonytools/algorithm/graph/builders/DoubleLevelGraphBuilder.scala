package pl.agh.harmonytools.algorithm.graph.builders

import pl.agh.harmonytools.algorithm.evaluator.ConnectionEvaluator
import pl.agh.harmonytools.algorithm.generator.LayerGenerator
import pl.agh.harmonytools.algorithm.graph.node.{Layer, Node}

abstract class DoubleLevelGraphBuilder[T, S, Q](firstContent: T, lastContent: T) {
  def createNestedLayer(node: Node[T], outerGeneratorInput: S, layerId: Int): Layer[T, S]
  // = new ChordGeneratorInput(node.getContent.harmonicFunction, layerId !=0, outerGeneratorInput.sopranoNote)

  private var outerEvaluator: Option[ConnectionEvaluator[T]] = None
  private var innerEvaluator: Option[ConnectionEvaluator[Q]] = None
  private var outerGenerator: Option[LayerGenerator[T, S]]   = None
  private var innerGenerator: Option[LayerGenerator[S, Q]]   = None
  private var outerGeneratorInputs: Option[List[S]]          = None

  def withOuterEvaluator(outerEvaluator: ConnectionEvaluator[T]): Unit = this.outerEvaluator = Some(outerEvaluator)
  def withInnerEvaluator(innerEvaluator: ConnectionEvaluator[Q]): Unit = this.innerEvaluator = Some(innerEvaluator)
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

  private def getGraphTemplate: SingleLevelGraphBuilder[T, S] = {
    val graphBuilder = new SingleLevelGraphBuilder[T, S](firstContent, lastContent)
    graphBuilder.withEvaluator(getOuterEvaluator)
    graphBuilder.withGenerator(getOuterGenerator)
    graphBuilder.withGeneratorInput(getOuterGeneratorInputs)
    graphBuilder.buildWithoutWeights()
  }

  private def generateNestedLayers(graphBuilder: SingleLevelGraphBuilder[T, S]): Unit = {
    for (layerId <- graphBuilder.getLayers.indices) {
      for (currentNode <- graphBuilder.getLayers(layerId).getNodeList) {
        val nestedLayer = createNestedLayer(currentNode, getOuterGeneratorInputs(layerId), layerId)
        currentNode //todo
      }
    }
  }

}
