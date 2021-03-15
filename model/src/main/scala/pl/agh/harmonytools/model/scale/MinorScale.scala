package pl.agh.harmonytools.model.scale

import pl.agh.harmonytools.model.key.Key
import pl.agh.harmonytools.model.note.BaseNote

case class MinorScale(private val minorKey: Key) extends Scale {
  override val key: Key = minorKey
}

object MinorScale extends ScaleCompanion {
  override val pitches: List[Int] = List(0, 2, 3, 5, 7, 8, 10)

  def apply(baseNote: BaseNote.BaseNoteType, tonicPitch: Int): MinorScale =
    MinorScale(Key(baseNote, tonicPitch))
}
