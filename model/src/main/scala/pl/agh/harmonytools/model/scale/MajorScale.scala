package pl.agh.harmonytools.model.scale

import pl.agh.harmonytools.model.key.Key
import pl.agh.harmonytools.model.note.BaseNote

case class MajorScale(private val majorKey: Key) extends Scale {
  override val pitches: List[Int] = List(0, 2, 4, 5, 7, 9, 11)
  override val key: Key = majorKey
}

object MajorScale {
  def apply(baseNote: BaseNote.BaseNoteType, tonicPitch: Int): MajorScale =
    MajorScale(Key(baseNote, tonicPitch))
}

