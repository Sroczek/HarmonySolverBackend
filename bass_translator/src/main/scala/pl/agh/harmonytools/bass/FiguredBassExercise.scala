package pl.agh.harmonytools.bass

import pl.agh.harmonytools.exercise.harmonics.Meter
import pl.agh.harmonytools.model.key.{Key, Mode}

case class FiguredBassExercise(
  mode: Mode.BaseMode,
  key: Key,
  meter: Meter,
  elements: List[FiguredBassElement],
  durations: List[Float]
)
