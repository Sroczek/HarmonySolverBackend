package pl.agh.harmonytools.bass

object AlterationType {
  def ccFromSymbol(last: Char): Option[ChordComponentType] = {
    if (last.isDigit) None
    else last match {
      case '>' => Some(LOWERED)
      case '<' => Some(ELEVATED)
      case _ => None
    }
  }

  sealed abstract class FiguredBassType(val value: String)

  case object SHARP extends FiguredBassType("#")
  case object FLAT extends FiguredBassType("b")
  case object NATURAL extends FiguredBassType("h")

  sealed abstract class ChordComponentType(val value: String)

  case object LOWERED extends ChordComponentType(">")
  case object ELEVATED extends ChordComponentType("<")
  case object CANCELLED extends ChordComponentType("h")
}
