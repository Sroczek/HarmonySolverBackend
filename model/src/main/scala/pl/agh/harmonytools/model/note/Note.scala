package pl.agh.harmonytools.model.note

import pl.agh.harmonytools.model.chord.ChordComponent
import pl.agh.harmonytools.model.util.ChordComponentManager
import pl.agh.harmonytools.utils.Extensions.ExtendedInt

case class Note(
  pitch: Int,
  baseNote: BaseNote.BaseNoteType,
  chordComponent: ChordComponent,
  duration: Float
) {
  override def toString: String = {
    "Pitch: " + pitch + " BaseNote: " + baseNote + " ChordComponent: " + chordComponent.toString
  }

  def isUpperThan(other: Note): Boolean = pitch > other.pitch

  def isLowerThan(other: Note): Boolean = pitch < other.pitch

  def chordComponentEquals(chordComponentString: String): Boolean = chordComponent.chordComponentString == chordComponentString

  def baseChordComponentEquals(baseComponentString: String): Boolean = chordComponent.baseComponent.toString == baseComponentString

  def equalPitches(other: Note): Boolean = pitch == other.pitch

  def equalsInOneOctave(other: Note): Boolean = {
    (pitch %% 12 == other.pitch %% 12) && baseNote == other.baseNote && chordComponent == other.chordComponent
  }
}

object Note {
  def apply(pitch: Int, baseNote: BaseNote.BaseNoteType, chordComponentString: String, duration: Float): Note = {
    Note(pitch, baseNote, ChordComponentManager.chordComponentFromString(chordComponentString), duration)
  }
}
