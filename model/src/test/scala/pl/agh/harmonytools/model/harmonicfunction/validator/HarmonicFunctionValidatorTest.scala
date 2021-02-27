package pl.agh.harmonytools.model.harmonicfunction.validator

import org.scalatest.{FunSuite, Matchers}
import pl.agh.harmonytools.model.harmonicfunction.FunctionNames.TONIC
import pl.agh.harmonytools.model.harmonicfunction.{Delay, HarmonicFunction}
import pl.agh.harmonytools.model.util.ChordComponentManager

class HarmonicFunctionValidatorTest extends FunSuite with Matchers {
  private val prime = ChordComponentManager.chordComponentFromString("1")
  private val third = ChordComponentManager.chordComponentFromString("3")
  private val fifth = ChordComponentManager.chordComponentFromString("5")
  private val seventh = ChordComponentManager.chordComponentFromString("7")
  private val ninth = ChordComponentManager.chordComponentFromString("8")

  private def validate(hf: HarmonicFunction): Unit = new HarmonicFunctionValidator(hf).validate()

  test("Too large difference in delay") {
    val hf1 = HarmonicFunction(baseFunction = TONIC, delay = List(Delay("8>", "9>")))
    val hf2 = HarmonicFunction(baseFunction = TONIC, delay = List(Delay("7>", "7<<")))
    val hf3 = HarmonicFunction(baseFunction = TONIC, delay = List(Delay("5>", "3")))
    validate(hf1)
    validate(hf2)
    assertThrows[HarmonicFunctionValidationError](validate(hf3))
  }

  test("Validate delay with omit, extra, revolution and position") {
    val hf1 = HarmonicFunction(baseFunction = TONIC, delay = List(Delay("4", "3")), extra = List(seventh))
    val hf2 = HarmonicFunction(baseFunction = TONIC, revolution = third, delay = List(Delay("4", "3")), extra = List(seventh))
    val hf3 = HarmonicFunction(baseFunction = TONIC, delay = List(Delay("4", "3")), extra = List(seventh, ninth), omit = List(fifth))
    val hf4 = HarmonicFunction(baseFunction = TONIC, delay = List(Delay("6", "5")), omit = List(fifth))
    List(hf1, hf2, hf3, hf4).foreach(validate)
  }
}
