package pl.agh.harmonytools.exercise.harmonics

import pl.agh.harmonytools.model.key.{Key, Mode}

case class HarmonicsExercise(
  key: Key,
  meter: Meter,
  measures: List[Measure]
) {
  lazy val mode: Mode.BaseMode = key.mode
}
