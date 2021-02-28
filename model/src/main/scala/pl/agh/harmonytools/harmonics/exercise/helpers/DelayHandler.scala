package pl.agh.harmonytools.harmonics.exercise.helpers

import pl.agh.harmonytools.model.harmonicfunction.HarmonicFunction

object DelayHandler {
  /**
   * Creates list of HarmonicFunction objects without abstract delays from given one containing abstract delays.
   * For each HarmonicFunction with abstract delays - it transforms this HarmonicFunction
   * to two HarmonicFunctions without abstract delays using `extra` and `omit` fields.
   * @param harmonicFunctionList list of harmonic functions with abstract delays
   * @return list of harmonic functions without abstract delays
   */
  def handleDelays(harmonicFunctionList: List[HarmonicFunction]): List[HarmonicFunction] = {
    var resultHfs: List[HarmonicFunction] = List.empty
    for (i <- harmonicFunctionList.indices) {
      var currentHf = harmonicFunctionList(i)
      if (currentHf.delay.nonEmpty) {
        var currentHfCopy = currentHf.copy()
        currentHfCopy = currentHfCopy.copy(delay = List())
        for (d <- currentHf.delay) {
          if (d.second.baseComponent >= 8 && !currentHfCopy.extra.contains(d.second))
            currentHfCopy = currentHfCopy.copy(extra = currentHfCopy.extra.appended(d.second))
          currentHf = currentHf.copy(extra = currentHf.extra.appended(d.first))
          currentHf = currentHf.copy(omit = currentHf.omit.appended(d.second))
          currentHf = currentHf.copy(extra = currentHf.extra.filter(_ != d.second))
          if (currentHf.position.isDefined && d.second == currentHf.position.get)
            currentHf = currentHf.copy(position = Some(d.first))
          if (d.second == currentHf.revolution) currentHf = currentHf.copy(revolution = d.first)
        }
        resultHfs = resultHfs.appendedAll(List(currentHf, currentHfCopy))
      } else resultHfs = resultHfs.appended(currentHf)
    }
    resultHfs
  }
}
