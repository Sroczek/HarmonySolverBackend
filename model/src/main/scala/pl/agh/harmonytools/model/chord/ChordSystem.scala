package pl.agh.harmonytools.model.chord

object ChordSystem {
  sealed trait System

  case object OPEN      extends System
  case object CLOSE     extends System
  case object UNDEFINED extends System

  def fromString(x: String): System = {
    x match {
      case "open"  => OPEN
      case "close" => CLOSE
      case _       => throw new IllegalArgumentException("Illegal system name: " + x)
    }
  }
}
