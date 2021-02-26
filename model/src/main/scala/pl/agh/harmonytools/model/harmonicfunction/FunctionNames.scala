package pl.agh.harmonytools.model.harmonicfunction

import pl.agh.harmonytools.model.scale.ScaleDegree

object FunctionNames {
  sealed abstract class BaseFunction(val name: String, val baseDegree: ScaleDegree.Degree)

  case object TONIC       extends BaseFunction("T", ScaleDegree.I)
  case object SUBDOMINANT extends BaseFunction("S", ScaleDegree.IV)
  case object DOMINANT    extends BaseFunction("D", ScaleDegree.V)

  def fromName(x: String): BaseFunction = {
    x match {
      case TONIC.name       => TONIC
      case SUBDOMINANT.name => SUBDOMINANT
      case DOMINANT.name    => DOMINANT
      case _                => throw new IllegalArgumentException("Illegal function name: " + x)
    }
  }
}
