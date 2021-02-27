package pl.agh.harmonytools.harmonics.parser

import org.scalatest.{Assertion, BeforeAndAfterEach, FunSuite, Ignore, Matchers}
import pl.agh.harmonytools.harmonics.exercise.HarmonicsExercise
import pl.agh.harmonytools.model.harmonicfunction.Delay
import pl.agh.harmonytools.model.harmonicfunction.FunctionNames.TONIC
import pl.agh.harmonytools.model.key.Key
import pl.agh.harmonytools.model.scale.ScaleDegree.VI
import pl.agh.harmonytools.model.util.ChordComponentManager

import scala.io.Source
import scala.reflect.ClassTag

class HarmonicsParserTest extends FunSuite with Matchers with BeforeAndAfterEach {

  private val EXERCISES_PATH = "/examples/1_HarmonicFunctions"

  private def testForSuccess(input: String): Assertion   = HarmonicsParserTest.success(input) shouldBe true
  private def testForNoSuccess(input: String): Assertion = HarmonicsParserTest.noSuccess(input) shouldBe true
  private def testToThrowWhileInitializingExercise[T <: Exception : ClassTag](input: String): Assertion =
    assertThrows[T](HarmonicsParserTest.parseInput(input))
  private def getParserOutput(input: String) = HarmonicsParserTest.parseInput(input)

  override def beforeEach(): Unit =
    HarmonicsParserTest.reset()

  private def getFileContent(filePath: String): String = {
    val source = Source.fromURL(getClass.getResource(EXERCISES_PATH + filePath))
    try source.mkString
    finally source.close()
  }

  test("Wrong delay notation") {
    testForNoSuccess("""C
        |4/4
        |T{delay:5,4}
        |""".stripMargin)
  }

  test("Illegal key:value relation") {
    testForNoSuccess("""C
        |4/4
        |T{delay:5:4}
        |""".stripMargin)
  }

  test("Invalid boolean property") {
    testForNoSuccess("""C
        |4/4
        |T{down:true}
        |""".stripMargin)
    testForNoSuccess("""C
        |4/4
        |T{isRelatedBackwards:true}
        |""".stripMargin)
  }

  test("Illegal property") {
    testForNoSuccess("""C
        |4/4
        |T{positio:5}
        |""".stripMargin)
  }

  test("Handling whitespaces") {
    testForSuccess(getFileContent("/major/whitespaces.txt"))
  }

  test("Deflection in last chord") {
    testToThrowWhileInitializingExercise[HarmonicsParserException](
      getFileContent("/major/deflection_in_last_chord.txt")
    )
  }

  test("Deflection inside another deflection test") {
    testToThrowWhileInitializingExercise[HarmonicsParserException](
      getFileContent("/major/deflection_inside_deflection.txt")
    )
  }

  test("Parentheses mismatch - unclosed deflection") {
    testToThrowWhileInitializingExercise[HarmonicsParserException](getFileContent("/major/unclosed_deflection.txt"))
  }

  test("Parentheses mismatch - unexpected end of deflection") {
    testToThrowWhileInitializingExercise[HarmonicsParserException](
      getFileContent("/major/unexpected_end_of_deflection.txt")
    )
  }

  test("Classic deflection to backward deflection") {
    testToThrowWhileInitializingExercise[HarmonicsParserException](
      getFileContent("/major/deflection_to_backward_deflection.txt")
    )
  }

  test("Empty deflection") {
    testForNoSuccess("""C
        |4/4
        |T{} () T{}""".stripMargin)
  }

  test("Backward deflection in first hf") {
    testToThrowWhileInitializingExercise[HarmonicsParserException](
      getFileContent("/major/deflection_backward_first_chord.txt")
    )
  }

  test("Correct example") {
    testForSuccess(getFileContent("/major/new_notation/example_correct.txt"))
  }

  test("Chained classic deflection") {
    val exercise = getParserOutput(getFileContent("/major/chained_deflection_basic.txt"))
    exercise.get.measures.head.harmonicFunctions(1).key shouldBe Some(Key("D"))
    exercise.get.measures.head.harmonicFunctions(2).key shouldBe Some(Key("G"))
  }

  test("Deflection between measures") {
    val exercise = getParserOutput(getFileContent("/minor/basic_deflection_between_measures.txt"))
    exercise.get.measures.head.harmonicFunctions(1).key shouldBe Some(Key("a"))
    exercise.get.measures(1).harmonicFunctions.head.key shouldBe Some(Key("a"))
  }

  test("Chained deflection backwards") {
    val exercise = getParserOutput(getFileContent("/major/deflection_backwards.txt"))
    exercise.get.measures.head.harmonicFunctions(2).key shouldBe Some(Key("F"))
    exercise.get.measures(1).harmonicFunctions.head.key shouldBe Some(Key("Bb"))
  }

  test("Deflection backwards between measures") {
    val exercise = getParserOutput(getFileContent("/major/deflection_backwards_between_measures.txt"))
    exercise.get.measures.head.harmonicFunctions(2).key shouldBe Some(Key("Bb"))
    exercise.get.measures(1).harmonicFunctions.head.key shouldBe Some(Key("Bb"))
  }

  test("Basic ellipse") {
    val exercise = getParserOutput(getFileContent("/major/elipse_correct.txt"))
    exercise.get.measures.head.harmonicFunctions(1).key shouldBe Some(Key("a"))
    exercise.get.measures.head.harmonicFunctions(2).key shouldBe Some(Key("a"))
    exercise.get.measures(2).harmonicFunctions(1).key shouldBe Some(Key("G"))
    exercise.get.measures(2).harmonicFunctions(2).key shouldBe Some(Key("G"))
    exercise.get.measures.head.harmonicFunctions(2).baseFunction shouldBe TONIC
    exercise.get.measures.head.harmonicFunctions(2).degree shouldBe VI
  }

  test("Ellipse inside deflection") {
    testToThrowWhileInitializingExercise[HarmonicsParserException]("""C
        |3/4
        |(T{} [S{}]) T{}
        |""".stripMargin)
  }

  test("More than one hf in ellipse") {
    testForNoSuccess("""
        |C
        |3/4
        |(D{}) [S{} D{}] T{}
        |""".stripMargin)
  }

  test("Empty ellipse") {
    testForNoSuccess("""
        |C
        |3/4
        |(D{}) [] T{}
        |""".stripMargin)
  }

  private lazy val simpleDelayExercise = getParserOutput("""C
      |3/4
      |T{delay:4-3}
      |""".stripMargin).get

  test("Dividing function into two - delays") {
    simpleDelayExercise.measures.head.harmonicFunctions.size shouldBe 2
  }

  test("Transformation of first child function correctness - delays") {
    val first = simpleDelayExercise.measures.head.harmonicFunctions.head
    first.omit shouldBe List(ChordComponentManager.chordComponentFromString("3"))
    first.extra shouldBe List(ChordComponentManager.chordComponentFromString("4"))
    first.delay shouldBe List(Delay("4", "3"))
  }

  test("Transformation of second child function correctness - delays") {
    val second = simpleDelayExercise.measures.head.harmonicFunctions(1)
    second.omit shouldBe List()
    second.extra shouldBe List()
    second.delay shouldBe List()
  }

  test("Transformation with delay and fixed position") {
    val exercise = getParserOutput("""C
        |3/4
        |T{delay:4-3/position:3}
        |""".stripMargin).get

    exercise.measures.head.harmonicFunctions.head.position shouldBe Some(
      ChordComponentManager.chordComponentFromString("4")
    )
    exercise.measures.head.harmonicFunctions(1).position shouldBe Some(
      ChordComponentManager.chordComponentFromString("3")
    )
  }

  test("Transformation with delay and fixed revolution") {
    val exercise = getParserOutput("""C
        |3/4
        |T{delay:4-3/revolution:3}
        |""".stripMargin).get

    exercise.measures.head.harmonicFunctions.head.revolution shouldBe ChordComponentManager.chordComponentFromString(
      "4"
    )
    exercise.measures.head.harmonicFunctions(1).revolution shouldBe ChordComponentManager.chordComponentFromString("3")
  }

  test("More measures with delayed functions") {
    val exercise = getParserOutput("""C
        |3/4
        |T{delay:4-3} T{delay:4-3}
        |T{delay:4-3}
        |T{delay:4-3} T{delay:4-3}
        |""".stripMargin).get
    exercise.measures.map(_.harmonicFunctions.length).sum shouldBe 10
  }

  test("Double delayed function transformation") {
    val exercise = getParserOutput("""C
        |3/4
        |T{delay:6-5,4-3}
        |""".stripMargin).get
    val first    = exercise.measures.head.harmonicFunctions.head
    first.extra shouldBe List(
      ChordComponentManager.chordComponentFromString("6"),
      ChordComponentManager.chordComponentFromString("4")
    )
    first.omit shouldBe List(
      ChordComponentManager.chordComponentFromString("5"),
      ChordComponentManager.chordComponentFromString("3")
    )
  }

  test("Triple delayed function transformation") {
    val exercise = getParserOutput("""C
        |3/4
        |T{delay:6-5,4-3,2-1}
        |""".stripMargin).get
    val first    = exercise.measures.head.harmonicFunctions.head
    first.extra shouldBe List(
      ChordComponentManager.chordComponentFromString("6"),
      ChordComponentManager.chordComponentFromString("4"),
      ChordComponentManager.chordComponentFromString("2")
    )
    first.omit shouldBe List(
      ChordComponentManager.chordComponentFromString("5"),
      ChordComponentManager.chordComponentFromString("3"),
      ChordComponentManager.chordComponentFromString("1")
    )
  }

  test("Exercise with empty lines") {
    testForSuccess(
      """
        |
        |
        |C
        |
        |
        |
        |4/4
        |
        |T{}
        |
        |
        |
        |S{}
        |
        |
        |D{}
        |""".stripMargin)
  }

}

object HarmonicsParserTest extends HarmonicsParser {
  override def reset(): Unit = super.reset()

  private def success(input: String): Boolean =
    parse(harmonicsExerciseDef, input).successful

  private def noSuccess(input: String): Boolean =
    !parse(harmonicsExerciseDef, input).successful

  private def parseInput(input: String): ParseResult[HarmonicsExercise] =
    parse(harmonicsExerciseDef, input)
}
