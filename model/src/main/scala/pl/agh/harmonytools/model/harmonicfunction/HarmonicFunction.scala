package pl.agh.harmonytools.model.harmonicfunction

import pl.agh.harmonytools.model.chord.{ChordComponent, ChordSystem}
import pl.agh.harmonytools.model.key.{Key, Mode}
import pl.agh.harmonytools.model.scale.ScaleDegree
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
) extends BasicComponentsOwner {

  override protected def getDegree: ScaleDegree.Degree  = degree
  override protected def getIsDown: Boolean             = isDown
  override protected def getMode: Mode.BaseMode         = mode
  override protected def getExtra: List[ChordComponent] = extra
  override protected def getOmit: List[ChordComponent]  = omit
  override protected def getDelay: List[Delay]          = delay
  override protected def getKey: Option[Key] = key
  override protected def getBaseFunction: FunctionNames.BaseFunction = baseFunction
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
      case Some(value) =>
        new HarmonicFunction(
          baseFunction,
          value,
          position,
          revolution,
          delay,
          extra,
          omit,
          isDown,
          system,
          mode,
          key,
          isRelatedBackwards
        )
      case None =>
        new HarmonicFunction(
          baseFunction,
          baseFunction.baseDegree,
          position,
          revolution,
          delay,
          extra,
          omit,
          isDown,
          system,
          mode,
          key,
          isRelatedBackwards
        )
    }
  }
}
