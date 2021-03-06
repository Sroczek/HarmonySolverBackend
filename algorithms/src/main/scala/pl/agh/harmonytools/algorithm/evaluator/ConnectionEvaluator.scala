package pl.agh.harmonytools.algorithm.evaluator

import pl.agh.harmonytools.algorithm.graph.node.NodeContent

trait ConnectionEvaluator[T <: NodeContent] {
  protected val connectionSize: Int
  protected val softRules: List[IRule[T]]
  protected val hardRules: List[IRule[T]]

  def getConnectionSize: Int = connectionSize

  def evaluateHardRules(connection: Connection[T]): Boolean
  def evaluateSoftRules(connection: Connection[T]): Int
}
