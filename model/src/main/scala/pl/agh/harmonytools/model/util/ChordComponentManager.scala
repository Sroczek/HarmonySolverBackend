package pl.agh.harmonytools.model.util

import pl.agh.harmonytools.model.chord.ChordComponent

import scala.collection.immutable.HashMap

object ChordComponentManager {

  private var availableChordComponents: HashMap[(String, Boolean), ChordComponent] = HashMap.empty

  def getRoot: ChordComponent = chordComponentFromString("1")

  def chordComponentFromString(chordComponentString: String, isDown: Boolean = false): ChordComponent = {
    availableChordComponents.find(_._1 == (chordComponentString, isDown)) match {
      case Some((key, value)) => value
      case None =>
        val cc = ChordComponent(chordComponentString, isDown)
        availableChordComponents = availableChordComponents + ((chordComponentString, isDown) -> cc)
        cc
    }
  }

  @deprecated
  def basicChordComponentFromPitch(pitch: Int, isDown: Boolean): ChordComponent = {
    pitch match {
      case 3 => chordComponentFromString("3>", isDown)
      case 4 => chordComponentFromString("3", isDown)
      case 5 => chordComponentFromString("3<", isDown)
      case 6 => chordComponentFromString("5>", isDown)
      case 7 => chordComponentFromString("5", isDown)
      case 8 => chordComponentFromString("5<", isDown)
    }
  }

}
