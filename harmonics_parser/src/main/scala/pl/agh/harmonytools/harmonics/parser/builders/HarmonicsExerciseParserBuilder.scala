package pl.agh.harmonytools.harmonics.parser.builders

import pl.agh.harmonytools.harmonics.exercise.{HarmonicsExercise, Meter}
import pl.agh.harmonytools.harmonics.exercise
import pl.agh.harmonytools.model.key.Key

class HarmonicsExerciseParserBuilder(
  private var key: Option[Key] = None,
  private var meter: Option[Meter] = None,
  private var measures: Option[List[MeasureParserBuilder]] = None
) {

  def withKey(k: Key): Unit                             = key = Some(k)
  def withMeter(m: Meter): Unit                         = meter = Some(m)
  def withMeasures(m: List[MeasureParserBuilder]): Unit = measures = Some(m)

  def getHarmonicsExercise: HarmonicsExercise = {

    //todo użyć walidatora tutaj, obsłużyć delaye, poustawiać key, wykonać czynności z "CONSTRUCTOR PART 2"

    exercise.HarmonicsExercise(
      key.getOrElse(sys.error("Key should be declared to initialize HarmonicsExercise")),
      meter.getOrElse(sys.error("Meter should be declared to initialize HarmonicsExercise")),
      measures.getOrElse(sys.error("Measures should be declared to initialize HarmonicsExercise")).map(_.getMeasure)
    )
  }

  override def toString: String =
    "HarmonicsExercise" + Seq(
      key.getOrElse("undefined").toString,
      meter.getOrElse("undefined").toString,
      measures.getOrElse("undefined").toString
    ).mkString("(", ",", ")")

}
