package pl.agh.harmonytools.model.scale

import pl.agh.harmonytools.model.key.Key

trait Scale {
  val key: Key
}

trait ScaleCompanion {
  val pitches: List[Int]
}
