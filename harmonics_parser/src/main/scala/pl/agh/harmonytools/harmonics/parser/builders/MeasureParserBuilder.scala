package pl.agh.harmonytools.harmonics.parser.builders

import pl.agh.harmonytools.harmonics.exercise.Measure

class MeasureParserBuilder(private var harmonicFunctions: Option[List[HarmonicFunctionParserBuilder]] = None) {
  def withHarmonicFunctions(hf: List[HarmonicFunctionParserBuilder]): Unit = harmonicFunctions = Some(hf)

  def getHarmonicFunctions: List[HarmonicFunctionParserBuilder] = harmonicFunctions.getOrElse(throw sys.error("HarmonicFunctions not defined yet"))

  def getMeasure: Measure =
    Measure(
      harmonicFunctions
        .getOrElse(sys.error("HarmonicFunction list should be defined to initialize Measure"))
        .map(_.getHarmonicFunction)
    )

  override def toString: String =
    "Measure" + harmonicFunctions.map(_.toString).mkString("(", ",", ")")
}
