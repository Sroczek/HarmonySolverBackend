package pl.agh.harmonytools.algorithm.evaluator

import pl.agh.harmonytools.algorithm.graph.node.NodeContent

trait IRule[T <: NodeContent] {
  def evaluate(connection: Connection[T]): Int
  def isBroken(connection: Connection[T]): Boolean
  def isNotBroken(connection: Connection[T]): Boolean = !isBroken(connection)
}
