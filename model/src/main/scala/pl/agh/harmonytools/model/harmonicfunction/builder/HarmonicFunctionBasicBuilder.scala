package pl.agh.harmonytools.model.harmonicfunction.builder
import pl.agh.harmonytools.model.chord.ChordComponent
import pl.agh.harmonytools.model.harmonicfunction.HarmonicFunction
import pl.agh.harmonytools.model.harmonicfunction.validator.HarmonicFunctionValidationError
import pl.agh.harmonytools.model.util.ChordComponentManager

class HarmonicFunctionBasicBuilder extends HarmonicFunctionBuilder(false) {

  private def getCC(cc: String): ChordComponent = ChordComponentManager.chordComponentFromString(cc, isDown)

  override protected def preprocessHarmonicFunction(): HarmonicFunction = {
    if (
      (extra.contains(getCC("9")) || extra.contains(getCC("9>")) || extra
        .contains(getCC("9<"))) && !extra.contains(getCC("7")) && !extra.contains(getCC("7<"))
    )
      withExtra(extra.appended(getCC("7")))
    position match {
      case Some(p) if !extra.contains(p) && !getBasicChordComponents.contains(p) => withExtra(extra.appended(p))
      case _                                                                     =>
    }
    if (!extra.contains(revolution) && !getBasicChordComponents.contains(revolution))
      withExtra(extra.appended(revolution))
    if ((extra.contains(getCC("5<")) || extra.contains(getCC("5>"))) && omit.contains(getCC("5")))
      withOmit(omit.appended(getCC("5")))
    if (omit.contains(getCC("1")) && revolution == getCC("1")) withRevolution(getThird)
    if (omit.contains(getCC("5"))) {
      val fifth = getCC("5")
      if (fifth != getFifth) {
        withOmit(omit.filter(_ != fifth))
        withOmit(omit.appended(getFifth))
      }
    }
    if (omit.contains(getCC("3"))) {
      val third = getCC("3")
      if (third != getThird) {
        withOmit(omit.filter(_ != third))
        withOmit(omit.appended(getThird))
      }
    }
    if (revolution == getCC("5")) withRevolution(getFifth)
    if (position.contains(getCC("5"))) withPosition(getFifth)

    if (extra.exists(_.baseComponent == 9) || delay.exists(_.first.baseComponent == 9)) {
      if (countChordComponents > 4) {
        val prime = getPrime
        val fifth = getFifth
        if (position.isDefined && position.get == revolution)
          throw HarmonicFunctionValidationError("Ninth chord could not have same position as revolution")
        if (position.isDefined && List(prime, fifth).contains(position.get) && List(prime, fifth).contains(revolution))
          throw HarmonicFunctionValidationError(
            "Ninth chord could not have both prime or fifth in position or revolution"
          )
        if (!omit.contains(fifth) && !position.contains(fifth) && revolution != fifth)
          withOmit(omit.appended(fifth))
        else if (!omit.contains(prime)) {
          withOmit(omit.appended(prime))
          if (revolution == prime)
            withRevolution(getThird)
        }
      }
    }

    initializeHarmonicFunction()
  }
}
