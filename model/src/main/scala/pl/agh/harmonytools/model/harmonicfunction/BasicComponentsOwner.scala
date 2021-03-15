package pl.agh.harmonytools.model.harmonicfunction

import pl.agh.harmonytools.model.chord.ChordComponent
import pl.agh.harmonytools.model.harmonicfunction.FunctionNames.{BaseFunction, TONIC}
import pl.agh.harmonytools.model.key.Mode.{MAJOR, MINOR}
import pl.agh.harmonytools.model.key.{Key, Mode}
import pl.agh.harmonytools.model.scale.ScaleDegree.VI
import pl.agh.harmonytools.model.scale.{MajorScale, MinorScale, ScaleDegree}
import pl.agh.harmonytools.model.util.ChordComponentManager
import pl.agh.harmonytools.utils.Extensions.ExtendedInt

trait BasicComponentsOwner {

  protected def getIsDown: Boolean
  protected def getMode: Mode.BaseMode
  protected def getExtra: List[ChordComponent]
  protected def getOmit: List[ChordComponent]
  protected def getDelay: List[Delay]
  protected def getDegree: ScaleDegree.Degree
  protected def getKey: Option[Key]
  protected def getBaseFunction: BaseFunction

  def isInDominantRelation(next: BasicComponentsOwner): Boolean = {
    if(getIsDown != next.getIsDown && getKey == next.getKey && !(getBaseFunction == TONIC
      && getDegree == VI
      && getMode == MINOR && next.getIsDown)) {
      false
    } else if (getKey != next.getKey && getKey.isDefined) {
      List(4,-3).contains(getDegree.root)
    } else if(getKey == next.getKey) {
      List(4,-3).contains(getDegree.root - next.getDegree.root)
    } else {
      false
    }
  }

  def getPrime: ChordComponent = ChordComponentManager.chordComponentFromString("1", getIsDown)
  def getThird: ChordComponent = {
    if (getIsDown) ChordComponentManager.chordComponentFromString("3", isDown = true)
    else {
      val pitches = if (getMode == MAJOR) MajorScale.pitches else MinorScale.pitches
      val thirdPitch = (pitches((getDegree.root + 1) % 7) - pitches(getDegree.root)) %% 12
      ChordComponentManager.basicChordComponentFromPitch(thirdPitch, isDown = false)
    }
  }
  def getFifth: ChordComponent = {
    if (getIsDown) ChordComponentManager.chordComponentFromString("5", isDown = true)
    else {
      val pitches = if (getMode == MAJOR) MajorScale.pitches else MinorScale.pitches
      val fifthPitch = (pitches((getDegree.root + 3) % 7) - pitches(getDegree.root)) %% 12
      ChordComponentManager.basicChordComponentFromPitch(fifthPitch, isDown = false)
    }
  }
  def getBasicChordComponents: List[ChordComponent] = List(getPrime, getThird, getFifth)

  def countChordComponents: Int = {
    var count = 3
    count += getExtra.length
    count -= getOmit.length
    for (d <- getDelay) {
      if (!getExtra.contains(d.first) && (getOmit.contains(d.second) || d.second.baseComponent == 8)) count += 1
      if (getExtra.contains(d.first) && (!getOmit.contains(d.second) && d.second.baseComponent != 8)) count -= 1
    }
    count
  }
}