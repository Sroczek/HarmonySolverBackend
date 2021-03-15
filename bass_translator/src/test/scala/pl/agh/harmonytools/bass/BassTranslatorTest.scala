package pl.agh.harmonytools.bass

import org.scalatest.{FunSuite, Matchers}
import pl.agh.harmonytools.model.harmonicfunction.FunctionNames.{BaseFunction, DOMINANT, SUBDOMINANT, TONIC}
import pl.agh.harmonytools.model.key.Key
import pl.agh.harmonytools.model.note.BaseNote.A

class BassTranslatorTest extends FunSuite with Matchers {

  private val noteBuilder = FiguredBassElement(NoteBuilder(0,A,0), List.empty, List.empty)

  test("Complete figured bass numbers") {
    def completeFiguredBassNumbersTest(symbols: List[Int], expectedCompletedSymbols: List[Int]) {
      val afterComplete = BassTranslator.completeFiguredBassNumbers(symbols).sortWith(_ < _)
      afterComplete shouldBe expectedCompletedSymbols.sortWith(_ < _)
    }

    completeFiguredBassNumbersTest(List.empty[Int], List(3, 5))
    completeFiguredBassNumbersTest(List(5), List(3, 5))
    completeFiguredBassNumbersTest(List(6), List(3, 6))
    completeFiguredBassNumbersTest(List(10, 2), List(10, 4, 2))
    completeFiguredBassNumbersTest(List(2), List(6, 4, 2))
    completeFiguredBassNumbersTest(List(4, 3), List(6, 4, 3))
    completeFiguredBassNumbersTest(List(7), List(7, 5, 3))
    completeFiguredBassNumbersTest(List(6, 5), List(6, 5, 3))
    completeFiguredBassNumbersTest(List(7, 5, 6), List(7, 5, 6))
  }

  test("Make choice and split") {
    val functions = List(
      List(TONIC, SUBDOMINANT),
      List(SUBDOMINANT),
      List(TONIC, SUBDOMINANT),
      List(DOMINANT),
      List(TONIC, SUBDOMINANT),
      List(SUBDOMINANT),
      List(TONIC, DOMINANT),
      List(SUBDOMINANT),
      List(TONIC, DOMINANT),
      List(SUBDOMINANT),
      List(TONIC, DOMINANT)
    )
    val actual = BassTranslator.makeChoiceAndSplit(functions.map(fs => {
      fs.map { f =>
        val builder = BassHarmonicFunctionBuilder()
        builder.withBaseFunction(f)
        builder
      }
    }))
    actual.length shouldBe functions.length
    for (i <- actual.dropRight(1).indices) {
      (actual(i), actual(i+1)) shouldNot be (DOMINANT, SUBDOMINANT)
    }
  }

  test("Has two next thirds") {
    def hasTwoNext3(notesNumbers: List[Int], expected: Boolean) = {
      ChordElement(notesNumbers, List.empty, noteBuilder).hasTwoNextThirds shouldBe expected
    }
    hasTwoNext3(List.empty[Int], expected = false)
    hasTwoNext3(List(3), expected = false)
    hasTwoNext3(List(1,2), expected = false)
    hasTwoNext3(List(0,1,2), expected = false)
    hasTwoNext3(List(0,2,3), expected = false)
    hasTwoNext3(List(0,2,4), expected = true)
    hasTwoNext3(List(1,3,6), expected = true)
  }

  test("Add next note") {
    def addNextNote(notesNumbers: List[Int], expectedNotesNumbers: List[Int]) = {
      val chordElement = ChordElement(notesNumbers, List.empty, noteBuilder)
      chordElement.addNextNote()
      chordElement.notesNumbers shouldBe expectedNotesNumbers
    }
    addNextNote(List(0,2), List(0,2,4))
    addNextNote(List(0,4), List(0,2,4))
    addNextNote(List(0,5), List(0,2,5))
    addNextNote(List(0,3), List(0,3,5))
  }

  test("Complete until two next thirds") {
    def complete(notesNumbers: List[Int], expectedNotesNumbers: List[Int]) = {
      val chordElement = ChordElement(notesNumbers, List.empty, noteBuilder)
      chordElement.completeUntilTwoNextThirds()
      chordElement.notesNumbers shouldBe expectedNotesNumbers
    }
    complete(List(0,2,4), List(0,2,4))
    complete(List(0), List(0,2,4))
    complete(List(0,2,10), List(0,2,4,10))
  }

  test("Get valid functions") {
    def getValidFunctionsTest(primeNote: Int, key: Key, expected: List[BaseFunction]) = {
      val chordElement = ChordElement(List.empty, List.empty, noteBuilder)
      chordElement.setPrimeNote(primeNote)
      BassTranslator.getValidFunctions(chordElement, key) shouldBe expected
    }
    getValidFunctionsTest(0, Key("C"), List(TONIC))
    getValidFunctionsTest(5, Key("C"), List(TONIC, SUBDOMINANT))
    getValidFunctionsTest(5, Key("a"), List(TONIC))
    getValidFunctionsTest(6, Key("Ab"), List(SUBDOMINANT))
    getValidFunctionsTest(2, Key("f"), List(DOMINANT))
  }

}
