package pl.agh.harmonytools.algorithm.generator

import pl.agh.harmonytools.algorithm.graph.node.NodeContent

trait LayerGenerator[T <: NodeContent, S <: GeneratorInput] {
  def generate(input: S): List[T]
}
