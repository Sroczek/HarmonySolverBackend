package pl.agh.harmonytools.harmonics.parser

import org.scalatest.{Assertion, FunSuite, Ignore, Matchers}
import pl.agh.harmonytools.harmonics.exercise.HarmonicsExercise

import scala.io.Source

class HarmonicsParserTest extends FunSuite with Matchers {

  private val EXERCISES_PATH = "/examples/1_HarmonicFunctions"

  private def testForSuccess(input: String): Assertion = HarmonicsParserTest.success(input) shouldBe true
  private def testForNoSuccess(input: String): Assertion = HarmonicsParserTest.noSuccess(input) shouldBe true

  private def getFileContent(filePath: String): String = {
    val source = Source.fromURL(getClass.getResource(EXERCISES_PATH + filePath))
    try source.mkString finally source.close()
  }

  test("Wrong delay notation") {
    testForNoSuccess(
      """C
        |4/4
        |T{delay:5,4}
        |""".stripMargin)
  }

  test("Illegal key:value relation") {
    testForNoSuccess(
      """C
        |4/4
        |T{delay:5:4}
        |""".stripMargin)
  }

  test("Invalid boolean property") {
    testForNoSuccess(
      """C
        |4/4
        |T{down:true}
        |""".stripMargin)
    testForNoSuccess(
      """C
        |4/4
        |T{isRelatedBackwards:true}
        |""".stripMargin)
  }

  test("Illegal property") {
    testForNoSuccess(
      """C
        |4/4
        |T{positio:5}
        |""".stripMargin)
  }

  test("Handling whitespaces") {
    testForSuccess(getFileContent("/major/whitespaces.txt"))
  }
}

object HarmonicsParserTest extends HarmonicsParser {
  private def success(input: String): Boolean = {
    parse(harmonicsExerciseDef, input).successful
  }

  private def noSuccess(input: String): Boolean = {
    !parse(harmonicsExerciseDef, input).successful
  }
}
