package pl.agh.harmonytools.model.scale

import pl.agh.harmonytools.model.key.Key
import pl.agh.harmonytools.model.note.BaseNote

case class MinorScale(private val minorKey: Key) extends Scale {
  override protected val key: Key = minorKey
  override protected val pitches: List[Int] = List(0, 2, 3, 5, 7, 8, 10)
}

object MinorScale {
  def apply(baseNote: BaseNote.BaseNoteType, tonicPitch: Int): MinorScale =
    MinorScale(Key(baseNote, tonicPitch))
}
