package pl.agh.harmonytools.model.harmonicfunction

import pl.agh.harmonytools.model._
import pl.agh.harmonytools.model.chord.{ChordComponent, ChordSystem}
import pl.agh.harmonytools.model.key.Mode.MAJOR
import pl.agh.harmonytools.model.key.{Key, Mode}
import pl.agh.harmonytools.model.scale.{MajorScale, MinorScale, ScaleDegree}
import pl.agh.harmonytools.model.util.ChordComponentManager
import pl.agh.harmonytools.utils.Extensions.ExtendedInt

case class HarmonicFunction(
  baseFunction: FunctionNames.BaseFunction,
  degree: ScaleDegree.Degree,
  position: Option[ChordComponent],
  revolution: ChordComponent,
  delay: List[Delay],
  extra: List[ChordComponent],
  omit: List[ChordComponent],
  isDown: Boolean,
  system: ChordSystem.System,
  mode: Mode.BaseMode,
  key: Option[Key],
  isRelatedBackwards: Boolean
) {
  def getPrime: ChordComponent = ChordComponentManager.chordComponentFromString("1", isDown)
  def getThird: ChordComponent = {
    if (isDown) ChordComponentManager.chordComponentFromString("3", isDown = true)
    else {
      val scale = if (mode == MAJOR) MajorScale(Key("C")) else MinorScale(Key("c"))
      val thirdPitch = (scale.pitches((degree.root + 1) % 7) - scale.pitches(degree.root - 1)) %% 12
      ChordComponentManager.basicChordComponentFromPitch(thirdPitch, isDown = false)
    }
  }
  def getFifth: ChordComponent = {
    if (isDown) ChordComponentManager.chordComponentFromString("5", isDown = true)
    else {
      val scale = if (mode == MAJOR) MajorScale(Key("C")) else MinorScale(Key("c"))
      val fifthPitch = (scale.pitches((degree.root + 3) % 7) - scale.pitches(degree.root - 1)) %% 12
      ChordComponentManager.basicChordComponentFromPitch(fifthPitch, isDown = false)
    }
  }
  def getBasicChordComponents: List[ChordComponent] = List(getPrime, getThird, getFifth)

  def countChordComponents: Int = {
    var count = 3
    count += extra.length
    count -= omit.length
    for (d <- delay) {
      if (!extra.contains(d.first) && (omit.contains(d.second) || d.second.baseComponent == 8)) count += 1
      if (extra.contains(d.first) && (!omit.contains(d.second) && d.second.baseComponent != 8)) count -= 1
    }
    count
  }
}

object HarmonicFunction {
  def apply(
    baseFunction: FunctionNames.BaseFunction,
    degree: Option[ScaleDegree.Degree] = None,
    position: Option[ChordComponent] = None,
    revolution: ChordComponent = ChordComponentManager.getRoot,
    delay: List[Delay] = List.empty,
    extra: List[ChordComponent] = List.empty,
    omit: List[ChordComponent] = List.empty,
    isDown: Boolean = false,
    system: ChordSystem.System = ChordSystem.UNDEFINED,
    mode: Mode.BaseMode = Mode.MAJOR,
    key: Option[Key] = None,
    isRelatedBackwards: Boolean = false
  ): HarmonicFunction = {
    degree match {
      case Some(value) => new HarmonicFunction(baseFunction, value, position, revolution, delay, extra, omit, isDown, system, mode, key, isRelatedBackwards)
      case None => new HarmonicFunction(baseFunction, baseFunction.baseDegree, position, revolution, delay, extra, omit, isDown, system, mode, key, isRelatedBackwards)
    }
  }
}
