package pl.agh.harmonytools.model.harmonicfunction.validator

import pl.agh.harmonytools.model.harmonicfunction.HarmonicFunction

class HarmonicFunctionValidator(private val hf: HarmonicFunction) {

  def validate(): Unit = {
    validateDelay()
    validateExtra()
    validateOmit()
    checkAllChordComponentNumber()
    checkIfExtraContainsPosition()
    checkIfExtraContainsRevolution()
  }

  private def validateDelay(): Unit = {
    val delays = hf.delay
    if (delays.length > 4) handleValidationFailure("Too large delay list - there are only 4 voices")
    for (delay <- delays)
      if (Math.abs(delay.first.baseComponent - delay.second.baseComponent) > 1)
        handleValidationFailure("Too large difference in delay")
  }

  private def validateExtra(): Unit = {
    val extras = hf.extra
    for (extra <- extras)
      if (hf.getBasicChordComponents.contains(extra))
        handleValidationFailure("Extra contains basic chord component which is not allowed")
    if (extras.toSet.size < extras.length) handleValidationFailure("Extra contains duplicates")
  }

  private def validateOmit(): Unit = {
    val omits = hf.omit
    for (omit <- omits)
      if (!hf.getBasicChordComponents.contains(omit) && omit.chordComponentString != "8")
        handleValidationFailure("Omit contains not basic chord component which is not allowed")
    if (omits.length == 2 && omits.head == omits.last) handleValidationFailure("Omit contains duplicates")
    if (omits.length > 2) handleValidationFailure("Omit is too large")
  }

  private def checkAllChordComponentNumber(): Unit =
    if (hf.countChordComponents > 4)
      handleValidationFailure("Count of chord components is too large - there are only 4 voices")

  private def checkIfExtraContainsPosition(): Unit =
    if (hf.position.isDefined && !hf.getBasicChordComponents.contains(hf.position.get) && !hf.extra.contains(hf.position.get))
      handleValidationFailure("Extra does not contain position which is not standard chord component")

  private def checkIfExtraContainsRevolution(): Unit = {
    if (!hf.getBasicChordComponents.contains(hf.revolution) && !hf.extra.contains(hf.revolution))
      handleValidationFailure("Extra does not contain revolution which is not standard chord component")
  }

  private def handleValidationFailure(msg: String): Unit = {
    throw HarmonicFunctionValidationError("HarmonicFunction validation error: " + msg)
  }
}

case class HarmonicFunctionValidationError(msg: String) extends IllegalArgumentException(msg)
