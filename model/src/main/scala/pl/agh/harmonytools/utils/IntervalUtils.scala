package pl.agh.harmonytools.utils

import pl.agh.harmonytools.model.key.{Key, Mode}
import pl.agh.harmonytools.model.key.Mode.{BaseMode, MAJOR, MINOR}
import pl.agh.harmonytools.model.scale.{MajorScale, MinorScale}
import pl.agh.harmonytools.model.scale.ScaleDegree.Degree
import pl.agh.harmonytools.utils.Extensions.ExtendedInt

object IntervalUtils {
  def getThirdMode(key: Key, degree: Degree): BaseMode = {
    val pitches = key.mode match {
      case Mode.MAJOR => MajorScale.pitches
      case Mode.MINOR => MinorScale.pitches
    }
    val baseValue = degree.root - 1
    val difference = Math.abs(pitches((baseValue + 2) %% 7) - pitches(baseValue))
    if (difference == 4 || difference == 8) MAJOR
    else MINOR
  }
}
