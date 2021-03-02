package pl.agh.harmonytools.algorithm.evaluator

trait ConnectionEvaluator[T] {
  protected val connectionSize: Int
  protected val softRules: List[IRule[T]]
  protected val hardRules: List[IRule[T]]

  def getConnectionSize: Int = connectionSize

  def evaluateHardRules(connection: Connection[T]): Boolean
  def evaluateSoftRules(connection: Connection[T]): Int
}
