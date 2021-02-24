package pl.agh.harmonytools.model

object ScaleDegree {
  sealed abstract class Degree(root: Int)

  case object I   extends Degree(1)
  case object II  extends Degree(2)
  case object III extends Degree(3)
  case object IV  extends Degree(4)
  case object V   extends Degree(5)
  case object VI  extends Degree(6)
  case object VII extends Degree(7)
}
