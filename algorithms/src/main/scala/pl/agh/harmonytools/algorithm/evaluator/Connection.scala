package pl.agh.harmonytools.algorithm.evaluator

import pl.agh.harmonytools.algorithm.graph.node.NodeContent

case class Connection[T <: NodeContent](
  current: T,
  prev: T,
  prevPrev: Option[T] = None
)
