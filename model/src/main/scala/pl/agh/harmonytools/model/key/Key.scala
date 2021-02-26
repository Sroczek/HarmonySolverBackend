package pl.agh.harmonytools.model.key

import pl.agh.harmonytools.model.key.Mode.BaseMode
import pl.agh.harmonytools.model.note.BaseNote

import scala.collection.immutable.HashMap

case class Key(
  mode: BaseMode,
  tonicPitch: Integer,
  baseNote: BaseNote.BaseNoteType
)

object Key {

  private def baseNoteFromKeySignature(keySignature: String): BaseNote.BaseNoteType =
    BaseNote.values
      .find(b => b.toString == keySignature.toUpperCase.head.toString)
      .getOrElse(throw new IllegalArgumentException("Unsupported key signature: " + keySignature))

  def apply(keySignature: String): Key = {
    Key(
      if (keySignature.head.isLower) Mode.MINOR else Mode.MAJOR,
      keyStrPitch.getOrElse(
        keySignature.toLowerCase,
        throw new IllegalArgumentException("Illegal keySignature: " + keySignature)
      ),
      baseNoteFromKeySignature(keySignature)
    )
  }

  def inferKeySignature(baseNote: BaseNote.BaseNoteType, tonicPitch: Integer): String = {
    val possibleKeySignatures = pitchKeyStr
      .getOrElse(tonicPitch, sys.error("Illegal tonicPitch: " + tonicPitch))
      .filter(keyCandidate => baseNoteFromKeySignature(keyCandidate).equals(baseNote))
      .toList
    possibleKeySignatures match {
      case Nil       => throw new IllegalArgumentException("Cannot infer a key")
      case el :: Nil => el
      case _         => throw new IllegalArgumentException("Cannot infer a key - too many possible keys for given arguments")
    }
  }

  def apply(baseNote: BaseNote.BaseNoteType, tonicPitch: Integer): Key = {
    Key(
      Mode.MAJOR,
      tonicPitch,
      baseNote
    )
  }

  private final val keyStrPitch: HashMap[String, Integer] = HashMap(
    "c"   -> 60,
    "b#"  -> 60,
    "dbb" -> 60,
    "c#"  -> 61,
    "db"  -> 61,
    "b##" -> 61,
    "d"   -> 62,
    "c##" -> 62,
    "ebb" -> 62,
    "d#"  -> 63,
    "eb"  -> 63,
    "fbb" -> 63,
    "e"   -> 64,
    "d##" -> 64,
    "fb"  -> 64,
    "f"   -> 65,
    "e#"  -> 65,
    "gbb" -> 65,
    "f#"  -> 66,
    "gb"  -> 66,
    "e##" -> 66,
    "g"   -> 67,
    "f##" -> 67,
    "abb" -> 67,
    "g#"  -> 68,
    "ab"  -> 68,
    "a"   -> 69,
    "g##" -> 69,
    "bbb" -> 69,
    "a#"  -> 70,
    "bb"  -> 70,
    "cbb" -> 70,
    "b"   -> 71,
    "cb"  -> 71,
    "a##" -> 71
  )

  private final val pitchKeyStr: HashMap[Integer, Set[String]] =
    HashMap.from(keyStrPitch.groupBy(_._2).view.mapValues(_.keySet))
}
