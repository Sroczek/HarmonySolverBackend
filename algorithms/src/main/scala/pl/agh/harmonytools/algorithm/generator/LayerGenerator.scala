package pl.agh.harmonytools.algorithm.generator

trait LayerGenerator[T, S] {
  def generate(input: S): List[T]
}
