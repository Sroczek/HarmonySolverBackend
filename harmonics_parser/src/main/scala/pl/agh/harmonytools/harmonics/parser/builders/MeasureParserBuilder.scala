package pl.agh.harmonytools.harmonics.parser.builders

import pl.agh.harmonytools.harmonics.exercise.Measure
import pl.agh.harmonytools.model.harmonicfunction.HarmonicFunction

class MeasureParserBuilder(private var harmonicFunctions: Option[List[HarmonicFunctionParserBuilder]] = None) {
  def withHarmonicFunctions(hf: List[HarmonicFunctionParserBuilder]): Unit = harmonicFunctions = Some(hf)

  def getHarmonicFunctions: List[HarmonicFunctionParserBuilder] =
    harmonicFunctions.getOrElse(throw sys.error("HarmonicFunctions not defined yet"))

  def getMeasure: Measure = {
    val hfs = harmonicFunctions
      .getOrElse(sys.error("HarmonicFunction list should be defined to initialize Measure")).map(_.getHarmonicFunction)
    var resultHfs: List[HarmonicFunction] = List.empty
    for (i <- hfs.indices) {
      var currentHf = hfs(i)
      if (currentHf.delay.nonEmpty) {
        var currentHfCopy = currentHf.copy()
        currentHfCopy = currentHfCopy.copy(delay = List())
        for (d <- currentHf.delay) {
          if (d.second.baseComponent >= 8 && !currentHfCopy.extra.contains(d.second)) {
            currentHfCopy = currentHfCopy.copy(extra = currentHfCopy.extra.appended(d.second))
          }
          currentHf = currentHf.copy(extra = currentHf.extra.appended(d.first))
          currentHf = currentHf.copy(omit = currentHf.omit.appended(d.second))
          currentHf = currentHf.copy(extra = currentHf.extra.filter(_ != d.second))
          if (currentHf.position.isDefined && d.second == currentHf.position.get) currentHf = currentHf.copy(position = Some(d.first))
          if (d.second == currentHf.revolution) currentHf = currentHf.copy(revolution = d.first)
        }
        resultHfs = resultHfs.appendedAll(List(currentHf, currentHfCopy))
      } else resultHfs = resultHfs.appended(currentHf)
    }

    Measure(resultHfs)
  }

  override def toString: String =
    "Measure" + harmonicFunctions.map(_.toString).mkString("(", ",", ")")
}
