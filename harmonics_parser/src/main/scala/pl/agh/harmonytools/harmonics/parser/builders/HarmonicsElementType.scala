package pl.agh.harmonytools.harmonics.parser.builders

sealed trait HarmonicsElementType

sealed trait Deflection extends HarmonicsElementType

sealed trait EllipseDeflection extends Deflection

sealed trait ClassicDeflection extends Deflection {
  def getNextType: ClassicDeflection
}

sealed trait BackwardDeflection extends Deflection {
  def getNextType: BackwardDeflection
}

case object ClassicDeflection1 extends ClassicDeflection {
  override def getNextType: ClassicDeflection = ClassicDeflection2
}

case object ClassicDeflection2 extends ClassicDeflection {
  override def getNextType: ClassicDeflection = ClassicDeflection1
}

case object BackwardDeflection1 extends BackwardDeflection {
  override def getNextType: BackwardDeflection = BackwardDeflection2
}

case object BackwardDeflection2 extends BackwardDeflection {
  override def getNextType: BackwardDeflection = BackwardDeflection1
}

case object Normal extends HarmonicsElementType

case object Ellipse extends EllipseDeflection
