package pl.agh.harmonytools.model

import pl.agh.harmonytools.model.util.ChordComponentManager

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
)

object HarmonicFunction {
  def apply(
    baseFunction: FunctionNames.BaseFunction,
    degree: Option[ScaleDegree.Degree],
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
