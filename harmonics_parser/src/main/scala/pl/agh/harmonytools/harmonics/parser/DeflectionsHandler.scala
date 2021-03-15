package pl.agh.harmonytools.harmonics.parser

import pl.agh.harmonytools.harmonics.parser.builders.{BackwardDeflection, ClassicDeflection, EllipseDeflection, HarmonicFunctionParserBuilder, HarmonicsElementType}
import pl.agh.harmonytools.model.harmonicfunction.FunctionNames.TONIC
import pl.agh.harmonytools.model.harmonicfunction.builder.HarmonicFunctionBuilder
import pl.agh.harmonytools.model.key.Mode.MINOR
import pl.agh.harmonytools.model.key.{Key, Mode}
import pl.agh.harmonytools.model.scale.{MajorScale, MinorScale, ScaleDegree}
import pl.agh.harmonytools.utils.IntervalUtils

import scala.annotation.tailrec

object DeflectionsHandler {
  @tailrec
  private def containsForbiddenDeflections(typeList: List[HarmonicsElementType]): Boolean = {
    typeList match {
      case Nil       => false
      case el :: Nil => false
      case t1 :: t2 :: tail =>
        if (t1.isInstanceOf[ClassicDeflection] && t2.isInstanceOf[BackwardDeflection]) true
        else containsForbiddenDeflections(t2 :: tail)
    }
  }

  @tailrec
  private def containsIllegalEllipse(typeList: List[HarmonicsElementType]): Boolean = {
    typeList match {
      case Nil      => false
      case t :: Nil => false
      case t1 :: t2 :: tail =>
        if (!t1.isInstanceOf[ClassicDeflection] && t2.isInstanceOf[EllipseDeflection]) true
        else containsIllegalEllipse(t2 :: tail)
    }
  }

  def handle(functions: List[HarmonicFunctionParserBuilder])(implicit exerciseKey: Key): Unit = {
    def sameType(a: HarmonicFunctionParserBuilder, b: HarmonicFunctionParserBuilder): Boolean = a.getType == b.getType

    val functionsGrouped = functions
      .drop(1)
      .foldLeft(List(List(functions.head)))((acc, e) =>
        if (sameType(e, acc.head.head)) (e :: acc.head) :: acc.tail else List(e) :: acc
      )
      .map(_.reverse)
      .reverse

    if (functionsGrouped.head.head.getType.isInstanceOf[BackwardDeflection])
      throw HarmonicsParserException("Backward deflection is illegal at the beginning")
    if (functionsGrouped.last.last.getType.isInstanceOf[ClassicDeflection])
      throw HarmonicsParserException("Classic deflection is illegal at the ending")
    if (functionsGrouped.head.head.getType.isInstanceOf[EllipseDeflection])
      throw HarmonicsParserException("Ellipse is forbidden at the beginning")
    if (containsForbiddenDeflections(functionsGrouped.map(_.head.getType)))
      throw HarmonicsParserException("Classic deflection to backward deflection is illegal")
    if (containsIllegalEllipse(functionsGrouped.map(_.head.getType)))
      throw HarmonicsParserException("Ellipse should be preceded by classic deflection")

    handleDeflections(functionsGrouped)
  }

  private def applyKeyToChords(key: Key, hfs: List[HarmonicFunctionParserBuilder]): Unit = hfs.foreach(_.withKey(key))

  private def handleDeflections(
    functionsGrouped: List[List[HarmonicFunctionParserBuilder]]
  )(implicit exerciseKey: Key): Unit = {
    for (i <- functionsGrouped.indices) {
      functionsGrouped(i).head.getType match {
        case _: BackwardDeflection =>
          val sourceChord = functionsGrouped(i - 1).last
          val key         = calculateKey(sourceChord)
          applyKeyToChords(key, functionsGrouped(i))
        case _: EllipseDeflection =>
          val ellipseChord = functionsGrouped(i).head
          val key          = calculateKey(ellipseChord)
          ellipseChord.withDegree(ScaleDegree.VI)
          ellipseChord.withBaseFunction(TONIC)
          applyKeyToChords(key, functionsGrouped(i))
        case _ =>
      }
    }
    for (i <- functionsGrouped.indices.reverse) {
      functionsGrouped(i).head.getType match {
        case _: ClassicDeflection =>
          val sourceChord = functionsGrouped(i + 1).head
          val key =
            if (sourceChord.getType.isInstanceOf[EllipseDeflection])
              sourceChord.getKey.getOrElse(sys.error("Found ellipse without declared key!"))
            else
              calculateKey(sourceChord)
          applyKeyToChords(key, functionsGrouped(i))
        case _ =>
      }
    }
  }

  def calculateKey(
    deflectionTargetHarmonicFunction: HarmonicFunctionBuilder
  )(implicit exerciseKey: Key): Key = {
    val keyToUse = deflectionTargetHarmonicFunction.getKey match {
      case Some(value) => value
      case None        => exerciseKey
    }
    val pitchesToUse = keyToUse.mode match {
      case Mode.MAJOR => MajorScale.pitches
      case Mode.MINOR => MinorScale.pitches
    }

    val degree   = deflectionTargetHarmonicFunction.getDegree
    var keyPitch = keyToUse.tonicPitch + pitchesToUse(degree.root)
    if (keyPitch >= 72) keyPitch = keyPitch - 12
    val keyBaseNote = keyToUse.baseNote + (degree.root)
    val modeToUse =
      if (deflectionTargetHarmonicFunction.getIsDown) MINOR else IntervalUtils.getThirdMode(keyToUse, degree)

    if (deflectionTargetHarmonicFunction.getIsDown) {
      keyPitch -= 1
      if (keyPitch < 60)
        keyPitch += 12
    }

    Key(modeToUse, keyPitch, keyBaseNote)
  }
}
