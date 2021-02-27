package pl.agh.harmonytools.model.harmonicfunction

import pl.agh.harmonytools.model._
import pl.agh.harmonytools.model.chord.{ChordComponent, ChordSystem}
import pl.agh.harmonytools.model.harmonicfunction.FunctionNames.BaseFunction
import pl.agh.harmonytools.model.harmonicfunction.validator.HarmonicFunctionValidator
import pl.agh.harmonytools.model.key.{Key, Mode}
import pl.agh.harmonytools.model.scale.ScaleDegree

trait HarmonicFunctionBuilder {
  def withBaseFunction(bf: BaseFunction): Unit

  def withDegree(d: ScaleDegree.Degree): Unit

  def withPosition(p: ChordComponent): Unit

  def withRevolution(r: ChordComponent): Unit

  def withDelay(d: List[Delay]): Unit

  def withExtra(e: List[ChordComponent]): Unit

  def withOmit(o: List[ChordComponent]): Unit

  def withIsDown(d: Boolean): Unit

  def withSystem(s: ChordSystem.System): Unit

  def withMode(m: Mode.BaseMode): Unit

  def withKey(k: Key): Unit

  def withIsRelatedBackwards(rb: Boolean): Unit

  protected def preprocessHarmonicFunction(): HarmonicFunction

  final def getHarmonicFunction: HarmonicFunction = {
    val hf = preprocessHarmonicFunction()
    new HarmonicFunctionValidator(hf).validate()
    hf
  }
}
