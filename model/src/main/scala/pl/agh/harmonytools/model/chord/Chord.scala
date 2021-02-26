package pl.agh.harmonytools.model.chord

import pl.agh.harmonytools.model.harmonicfunction.HarmonicFunction
import pl.agh.harmonytools.model.note.Note

case class Chord(
  sopranoNote: Note,
  altoNote: Note,
  tenorNote: Note,
  bassNote: Note,
  harmonicFunction: HarmonicFunction
) {
  lazy val notes: List[Note] = List(sopranoNote, altoNote, tenorNote, bassNote)

  override def toString: String = {
    s"""CHORD:\n
    Soprano note: ${sopranoNote.toString}\n
    Alto note: ${altoNote.toString}\n
    Tenor note: ${tenorNote.toString}\n
    Bass note: ${bassNote.toString}\n"""
  }

  def shortString: String = notes.map(_.pitch).mkString("|")

  def equalsNotes(other: Chord): Boolean =
    sopranoNote == other.sopranoNote && altoNote == other.altoNote &&
      tenorNote == other.tenorNote && bassNote == other.bassNote
}
