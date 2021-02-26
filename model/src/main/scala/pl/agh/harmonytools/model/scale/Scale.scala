package pl.agh.harmonytools.model.scale

import pl.agh.harmonytools.model.key.Key

trait Scale {
  protected val key: Key
  protected val pitches: List[Int]
}
