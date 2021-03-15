package pl.agh.harmonytools.bass

import pl.agh.harmonytools.bass.AlterationType.{ChordComponentType, ELEVATED, LOWERED}
import pl.agh.harmonytools.model.chord.ChordComponent
import pl.agh.harmonytools.model.util.ChordComponentManager

case class BassSymbol(var component: Int = 3, alteration: Option[AlterationType.FiguredBassType] = None) {
  def mapToChordComponentSymbol(): ChordComponentSymbol = {
    val alt = alteration flatMap[ChordComponentType] {
      case AlterationType.SHARP => Some(AlterationType.ELEVATED)
      case AlterationType.FLAT => Some(AlterationType.LOWERED)
      case AlterationType.NATURAL => Some(AlterationType.CANCELLED)
    }
    ChordComponentSymbol(component, alt)
  }
}

case class ChordComponentSymbol(var component: Int, var alteration: Option[AlterationType.ChordComponentType] = None) {

  def this(s: String) {
    this(s.takeWhile(_.isDigit).toInt, AlterationType.ccFromSymbol(s.last))
  }

  def decreaseByHalfTone(): Unit = {
    alteration match {
      case Some(alt) =>
        if (alt == ELEVATED) {
          alteration = None
        } else {
          alteration = Some(LOWERED)
        }
      case None => alteration = Some(LOWERED)
    }
  }

  def increaseByHalfTone(): Unit = {
    alteration match {
      case Some(alt) =>
        if (alt == LOWERED) {
          alteration = None
        } else {
          alteration = Some(ELEVATED)
        }
      case None => alteration = Some(ELEVATED)
    }
  }

  def setAlteration(alt: AlterationType.ChordComponentType): Unit = {
    alteration = Some(alt)
  }

  override def toString: String = {
    component.toString + {
      alteration match {
        case Some(alt) => alt.value
        case None => ""
      }
    }
  }

  def toChordComponent: ChordComponent = {
    ChordComponentManager.chordComponentFromString(toString)
  }
}
