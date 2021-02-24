package pl.agh.harmonytools.model

object FunctionNames {
  sealed abstract class BaseFunction(val name: String, val baseDegree: ScaleDegree.Degree)

  case object TONIC       extends BaseFunction("T", ScaleDegree.I)
  case object SUBDOMINANT extends BaseFunction("S", ScaleDegree.IV)
  case object DOMINANT    extends BaseFunction("D", ScaleDegree.V)
}
