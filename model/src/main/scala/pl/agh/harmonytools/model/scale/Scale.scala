package pl.agh.harmonytools.model.scale

import pl.agh.harmonytools.model.key.Key

trait Scale {
  val key: Key
  val pitches: List[Int]
}
