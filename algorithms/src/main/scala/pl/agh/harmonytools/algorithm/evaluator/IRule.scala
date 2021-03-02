package pl.agh.harmonytools.algorithm.evaluator

trait IRule[T] {
  def evaluate(connection: Connection[T]): Int
  def isBroken(connection: Connection[T]): Boolean
  def isNotBroken(connection: Connection[T]): Boolean = !isBroken(connection)
}
