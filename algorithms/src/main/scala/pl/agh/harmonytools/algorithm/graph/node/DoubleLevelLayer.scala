package pl.agh.harmonytools.algorithm.graph.node

import pl.agh.harmonytools.algorithm.generator.LayerGenerator

class DoubleLevelLayer[T <: NodeContent, S](private var nodeList: List[NodeWithNestedLayer[T, S]])
  extends Layer[T, S](nodeList) {
  def this(generatorInput: S, generator: LayerGenerator[T, S]) = {
    this(generator.generate(generatorInput).map(new NodeWithNestedLayer[T, S](_)))
  }

  override def getNodeList: List[NodeWithNestedLayer[T, S]] = nodeList
}
