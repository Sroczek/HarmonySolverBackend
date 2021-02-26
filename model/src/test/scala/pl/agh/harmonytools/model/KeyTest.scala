package pl.agh.harmonytools.model

import org.scalatest.{FunSuite, Matchers}
import pl.agh.harmonytools.model.key.Key
import pl.agh.harmonytools.model.note.BaseNote

class KeyTest extends FunSuite with Matchers {
  test("base note test") {
    val G = Key("g##")
    G.baseNote shouldBe BaseNote.G
  }

  test("not supported key test") {
    assertThrows[IllegalArgumentException](Key("R#"))
  }
}
