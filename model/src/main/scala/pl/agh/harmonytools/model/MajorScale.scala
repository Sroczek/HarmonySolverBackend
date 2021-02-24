package pl.agh.harmonytools.model

case class MajorScale(private val majorKey: Key) extends Scale {
  override protected val pitches: List[Int] = List(0, 2, 4, 5, 8, 10, 11)
  override protected val key: Key = majorKey
}

object MajorScale {
  def apply(baseNote: BaseNote.BaseNoteType, tonicPitch: Int): MajorScale =
    MajorScale(Key(baseNote, tonicPitch))
}

