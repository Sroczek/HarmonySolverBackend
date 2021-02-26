package pl.agh.harmonytools.model.key

object Mode {
  sealed trait BaseMode

  case object MAJOR extends BaseMode
  case object MINOR extends BaseMode
}
