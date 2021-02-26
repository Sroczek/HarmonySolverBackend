package pl.agh.harmonytools.harmonics.parser.builders

import pl.agh.harmonytools.model.harmonicfunction.HarmonicFunctionBuilder
import pl.agh.harmonytools.model.harmonicfunction.FunctionNames.BaseFunction
import pl.agh.harmonytools.model._
import pl.agh.harmonytools.model.chord.{ChordComponent, ChordSystem}
import pl.agh.harmonytools.model.harmonicfunction.{Delay, FunctionNames, HarmonicFunction, HarmonicFunctionBuilder}
import pl.agh.harmonytools.model.key.{Key, Mode}
import pl.agh.harmonytools.model.scale.ScaleDegree
import pl.agh.harmonytools.model.util.ChordComponentManager

class HarmonicFunctionParserBuilder extends HarmonicFunctionBuilder {
  private var baseFunction: Option[FunctionNames.BaseFunction] = None
  private var degree: Option[ScaleDegree.Degree]               = None
  private var position: Option[ChordComponent]                 = None
  private var revolution: ChordComponent                       = ChordComponentManager.getRoot
  private var delay: List[Delay]                               = List.empty
  private var extra: List[ChordComponent]                      = List.empty
  private var omit: List[ChordComponent]                       = List.empty
  private var isDown: Boolean                                  = false
  private var system: ChordSystem.System                       = ChordSystem.UNDEFINED
  private var mode: Mode.BaseMode                              = Mode.MAJOR
  private var key: Option[Key]                                 = None
  private var isRelatedBackwards: Boolean                      = false

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

  def getHarmonicFunction: HarmonicFunction =
    HarmonicFunction(
      baseFunction.getOrElse(sys.error("Base Function has to be defined to initialize HarmonicFunction")),
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

  override def toString: String =
    "HarmonicFunction" + Seq(
      baseFunction.getOrElse("undefined"),
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
    ).mkString("(", ",", ")")
}
