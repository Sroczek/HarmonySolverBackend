package pl.agh.harmonytools.model.util

import org.scalatest.{FunSuite, Matchers}

class ChordComponentManagerTest extends FunSuite with Matchers {
  test("instances check") {
    val cc1 = ChordComponentManager.chordComponentFromString("5>")
    val cc2 = ChordComponentManager.chordComponentFromString("5>")
    cc1 shouldBe cc2
    val cc3 = ChordComponentManager.chordComponentFromString("5>", isDown = true)
    val cc4 = ChordComponentManager.chordComponentFromString("5>")
    cc3 should not be cc4
    val cc5 = ChordComponentManager.chordComponentFromString("5>", isDown = true)
    val cc6 = ChordComponentManager.chordComponentFromString("5>", isDown = true)
    cc5 shouldBe cc6
  }
}
