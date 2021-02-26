package pl.agh.harmonytools.model.note

object BaseNote extends Enumeration {
  case class BaseNote(value: Int) extends Val
  type BaseNoteType = Value

  val C: BaseNote = BaseNote(0)
  val D: BaseNote = BaseNote(1)
  val E: BaseNote = BaseNote(2)
  val F: BaseNote = BaseNote(3)
  val G: BaseNote = BaseNote(4)
  val A: BaseNote = BaseNote(5)
  val B: BaseNote = BaseNote(6)
}
