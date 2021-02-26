package pl.agh.harmonytools.harmonics.parser

import pl.agh.harmonytools.harmonics.parser.builders.{
  BackwardDeflection,
  ClassicDeflection,
  HarmonicFunctionParserBuilder,
  HarmonicsElementType
}

import scala.annotation.tailrec

object DeflectionsHandler {
  @tailrec
  private def containsForbiddenDeflections(typeList: List[HarmonicsElementType]): Boolean = {
    typeList match {
      case Nil       => false
      case el :: Nil => false
      case t1 :: t2 :: tail =>
        if (t1.isInstanceOf[ClassicDeflection] && t2.isInstanceOf[BackwardDeflection]) true
        else containsForbiddenDeflections(tail)
    }
  }

  def handle(functions: List[HarmonicFunctionParserBuilder]): Unit = {
    def sameType(a: HarmonicFunctionParserBuilder, b: HarmonicFunctionParserBuilder): Boolean = a.getType == b.getType

    val functionsGrouped = functions
      .drop(1)
      .foldLeft(List(List(functions.head)))((acc, e) =>
        if (sameType(e, acc.head.head)) (e :: acc.head) :: acc.tail else List(e) :: acc
      )
      .map(_.reverse)
      .reverse

    if (functionsGrouped.head.head.getType.isInstanceOf[BackwardDeflection])
      throw new IllegalArgumentException("Backward deflection is illegal at the beginning")
    if (functionsGrouped.last.last.getType.isInstanceOf[ClassicDeflection])
      throw new IllegalArgumentException("Classic deflection is illegal at the ending")
    if (containsForbiddenDeflections(functionsGrouped.map(_.head.getType)))
      throw new IllegalArgumentException("Classic deflection to backward deflection is illegal")

  }
}
