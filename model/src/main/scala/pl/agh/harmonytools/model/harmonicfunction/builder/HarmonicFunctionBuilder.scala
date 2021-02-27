package pl.agh.harmonytools.model.harmonicfunction.builder

import pl.agh.harmonytools.model.chord.{ChordComponent, ChordSystem}
import pl.agh.harmonytools.model.harmonicfunction.FunctionNames.BaseFunction
import pl.agh.harmonytools.model.harmonicfunction.validator.HarmonicFunctionValidator
import pl.agh.harmonytools.model.harmonicfunction.{BasicComponentsOwner, Delay, FunctionNames, HarmonicFunction}
import pl.agh.harmonytools.model.key.{Key, Mode}
import pl.agh.harmonytools.model.scale.ScaleDegree
import pl.agh.harmonytools.model.util.ChordComponentManager

abstract class HarmonicFunctionBuilder(withValidation: Boolean = true) extends BasicComponentsOwner {
  protected var baseFunction: Option[FunctionNames.BaseFunction] = None
  protected var degree: Option[ScaleDegree.Degree]               = None
  protected var position: Option[ChordComponent]                 = None
  protected var revolution: ChordComponent                       = ChordComponentManager.getRoot
  protected var delay: List[Delay]                               = List.empty
  protected var extra: List[ChordComponent]                      = List.empty
  protected var omit: List[ChordComponent]                       = List.empty
  protected var isDown: Boolean                                  = false
  protected var system: ChordSystem.System                       = ChordSystem.UNDEFINED
  protected var mode: Mode.BaseMode                              = Mode.MAJOR
  protected var key: Option[Key]                                 = None
  protected var isRelatedBackwards: Boolean                      = false

  override def getDegree: ScaleDegree.Degree =
    degree match {
      case Some(value) => value
      case None        => baseFunction.getOrElse(sys.error("Base Function undefined")).baseDegree
    }
  override def getIsDown: Boolean                       = isDown
  override def getMode: Mode.BaseMode         = mode
  override def getExtra: List[ChordComponent] = extra
  override def getOmit: List[ChordComponent]  = omit
  override def getDelay: List[Delay]          = delay

  def withBaseFunction(bf: BaseFunction): Unit  = baseFunction = Some(bf)
  def withDegree(d: ScaleDegree.Degree): Unit   = degree = Some(d)
  def withPosition(p: ChordComponent): Unit     = position = Some(p)
  def withRevolution(r: ChordComponent): Unit   = revolution = r
  def withDelay(d: List[Delay]): Unit           = delay = d
  def withExtra(e: List[ChordComponent]): Unit  = extra = e
  def withOmit(o: List[ChordComponent]): Unit   = omit = o
  def withIsDown(d: Boolean): Unit              = isDown = d
  def withSystem(s: ChordSystem.System): Unit   = system = s
  def withMode(m: Mode.BaseMode): Unit          = mode = m
  def withKey(k: Key): Unit                     = key = Some(k)
  def withIsRelatedBackwards(rb: Boolean): Unit = isRelatedBackwards = rb

  protected def preprocessHarmonicFunction(): HarmonicFunction

  protected def initializeHarmonicFunction(): HarmonicFunction = {
    HarmonicFunction(
      baseFunction.getOrElse(sys.error("Base function has to be defined when initializing HarmonicFunction")),
      degree,
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

  final def getHarmonicFunction: HarmonicFunction = {
    val hf = preprocessHarmonicFunction()
    if (withValidation) new HarmonicFunctionValidator(hf).validate()
    hf
  }
}
