package pl.agh.harmonytools.model.harmonicfunction.builder

import org.scalatest.{FunSuite, Matchers}
import pl.agh.harmonytools.model.harmonicfunction.Delay
import pl.agh.harmonytools.model.harmonicfunction.FunctionNames.DOMINANT
import pl.agh.harmonytools.model.harmonicfunction.validator.HarmonicFunctionValidationError
import pl.agh.harmonytools.model.util.ChordComponentManager

class HarmonicFunctionBasicBuilderTest extends FunSuite with Matchers {

  private def constructNinthChordBuilder(position: String, revolution: String): HarmonicFunctionBasicBuilder = {
    val builder = new HarmonicFunctionBasicBuilder
    builder.withBaseFunction(DOMINANT)
    builder.withExtra(List(ChordComponentManager.chordComponentFromString("9")))
    builder.withPosition(ChordComponentManager.chordComponentFromString(position))
    builder.withRevolution(ChordComponentManager.chordComponentFromString(revolution))
    builder
  }

  test("Correct 5 adding to omit when applying ninth chord with delay") {
    val builder = new HarmonicFunctionBasicBuilder
    builder.withBaseFunction(DOMINANT)
    builder.withDelay(List(Delay("9", "8")))
    builder.withExtra(List(ChordComponentManager.chordComponentFromString("7")))
    builder.getHarmonicFunction.omit.contains(ChordComponentManager.chordComponentFromString("5"))
  }

  test("Validate ninth chords") {
    val builder1 = constructNinthChordBuilder("1", "5")
    val builder2 = constructNinthChordBuilder("1", "1")
    val builder3 = constructNinthChordBuilder("5", "1")
    val builder4 = constructNinthChordBuilder("5", "5")
    val builder5 = constructNinthChordBuilder("5>", "5>")
    Seq(builder1, builder2, builder3, builder4, builder5).foreach { builder =>
      assertThrows[HarmonicFunctionValidationError](builder.getHarmonicFunction)
    }
  }
}
