package pl.agh.harmonytools.model

object ChordSystem {
  sealed trait System

  case object OPEN extends System
  case object CLOSE extends System
  case object UNDEFINED extends System
}
