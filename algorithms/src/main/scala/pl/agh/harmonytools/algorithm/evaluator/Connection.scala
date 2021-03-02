package pl.agh.harmonytools.algorithm.evaluator

case class Connection[T](
  current: T,
  prev: T,
  prevPrev: Option[T] = None
)
