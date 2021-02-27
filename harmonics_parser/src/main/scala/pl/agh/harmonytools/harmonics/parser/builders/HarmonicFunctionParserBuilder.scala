package pl.agh.harmonytools.harmonics.parser.builders

import pl.agh.harmonytools.model.harmonicfunction.FunctionNames.BaseFunction
import pl.agh.harmonytools.model.chord.{ChordComponent, ChordSystem}
import pl.agh.harmonytools.model.harmonicfunction.{Delay, FunctionNames, HarmonicFunction, HarmonicFunctionBuilder}
import pl.agh.harmonytools.model.key.{Key, Mode}
import pl.agh.harmonytools.model.scale.ScaleDegree
import pl.agh.harmonytools.model.util.ChordComponentManager

sealed trait HarmonicsElementType
sealed trait Deflection        extends HarmonicsElementType
sealed trait EllipseDeflection extends Deflection
sealed trait ClassicDeflection extends Deflection {
  def getNextType: ClassicDeflection
}
sealed trait BackwardDeflection extends Deflection {
  def getNextType: BackwardDeflection
}
case object ClassicDeflection1 extends ClassicDeflection {
  override def getNextType: ClassicDeflection = ClassicDeflection2
}
case object ClassicDeflection2 extends ClassicDeflection {
  override def getNextType: ClassicDeflection = ClassicDeflection1
}
case object BackwardDeflection1 extends BackwardDeflection {
  override def getNextType: BackwardDeflection = BackwardDeflection2
}
case object BackwardDeflection2 extends BackwardDeflection {
  override def getNextType: BackwardDeflection = BackwardDeflection1
}
case object Normal  extends HarmonicsElementType
case object Ellipse extends EllipseDeflection

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
  private var hfType: HarmonicsElementType                     = Normal

  override def withBaseFunction(bf: BaseFunction): Unit  = baseFunction = Some(bf)
  override def withDegree(d: ScaleDegree.Degree): Unit   = degree = Some(d)
  override def withPosition(p: ChordComponent): Unit     = position = Some(p)
  override def withRevolution(r: ChordComponent): Unit   = revolution = r
  override def withDelay(d: List[Delay]): Unit           = delay = d
  override def withExtra(e: List[ChordComponent]): Unit  = extra = e
  override def withOmit(o: List[ChordComponent]): Unit   = omit = o
  override def withIsDown(d: Boolean): Unit              = isDown = d
  override def withSystem(s: ChordSystem.System): Unit   = system = s
  override def withMode(m: Mode.BaseMode): Unit          = mode = m
  override def withKey(k: Key): Unit                     = key = Some(k)
  override def withIsRelatedBackwards(rb: Boolean): Unit = isRelatedBackwards = rb
  def withType(t: HarmonicsElementType): Unit   = hfType = t

  def getIsRelatedBackwards: Boolean = isRelatedBackwards
  def getKey: Option[Key]            = key
  def getType: HarmonicsElementType  = hfType
  def getDegree: ScaleDegree.Degree =
    degree match {
      case Some(value) => value
      case None        => baseFunction.getOrElse(sys.error("Base Function undefined")).baseDegree
    }
  def getIsDown: Boolean = isDown

  override def preprocessHarmonicFunction(): HarmonicFunction =
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
      isRelatedBackwards,
      hfType
    ).mkString("(", ",", ")")
}
