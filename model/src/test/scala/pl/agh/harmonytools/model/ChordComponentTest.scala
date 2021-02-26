package pl.agh.harmonytools.model

import org.scalatest.{Assertion, FunSuite, Matchers}
import pl.agh.harmonytools.model.chord.ChordComponent

class ChordComponentTest extends FunSuite with Matchers {
  test("base component correct set") {
    def testBaseComponent(chordComponentString: String, expectedBaseComponent: Int): Assertion = {
      val cc = ChordComponent(chordComponentString)
      cc.baseComponent shouldBe expectedBaseComponent
    }

    val testCases = Seq(
      "3" -> 3,
      "13" -> 13,
      "13>" -> 13,
      "7>" -> 7,
      "7>>" -> 7,
      "7<" -> 7,
      "7<<" -> 7,
      ">7" -> 7,
      ">>7" -> 7,
      "<7" -> 7,
      "<<7" -> 7
    )

    testCases.foreach(el => testBaseComponent(el._1, el._2))
  }

  test("semitones number correct set") {
    def testSemitonesNumber(chordComponentString: String, expectedSemitonesNumber: Int): Assertion = {
      val cc = ChordComponent(chordComponentString)
      cc.semitonesNumber shouldBe expectedSemitonesNumber
    }

    val testCases = Seq(
      "1" -> 0,
      "1<" -> 1,
      "2" -> 2,
      "2>" -> 1,
      "2<" -> 3,
      "2>>" -> 0,
      "2<<" -> 4
    )

    testCases.foreach(el => testSemitonesNumber(el._1, el._2))
  }
}
