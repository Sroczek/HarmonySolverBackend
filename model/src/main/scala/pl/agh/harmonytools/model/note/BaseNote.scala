package pl.agh.harmonytools.model.note

import pl.agh.harmonytools.utils.Extensions.ExtendedInt

object BaseNote  {
  sealed abstract class BaseNoteType(val value: Int) {
    def +(x: Int): BaseNoteType = {
      fromInt((value + x) %% 7)
    }
  }

  case object C extends BaseNoteType(0)
  case object D extends BaseNoteType(1)
  case object E extends BaseNoteType(2)
  case object F extends BaseNoteType(3)
  case object G extends BaseNoteType(4)
  case object A extends BaseNoteType(5)
  case object B extends BaseNoteType(6)

  val values: List[BaseNoteType] = List(C, D, E, F, G, A, B)

  def fromInt(x: Int): BaseNoteType = {
    require(0 <= x && x < 7, "Base note should be from [0,6]")
    x match {
      case 0 => C
      case 1 => D
      case 2 => E
      case 3 => F
      case 4 => G
      case 5 => A
      case 6 => B
    }
  }
}
