package pl.agh.harmonytools.harmonics.parser

import pl.agh.harmonytools.harmonics.parser.builders.{
  HarmonicFunctionParserBuilder,
  HarmonicsExerciseParserBuilder,
  MeasureParserBuilder
}
import pl.agh.harmonytools.model.chord.ChordSystem.System
import pl.agh.harmonytools.model.harmonicfunction.FunctionNames._
import pl.agh.harmonytools.harmonics.exercise.{HarmonicsExercise, Meter}
import pl.agh.harmonytools.model.key.Mode.BaseMode
import pl.agh.harmonytools.model.scale.ScaleDegree.Degree
import pl.agh.harmonytools.model._
import pl.agh.harmonytools.model.chord.{ChordComponent, ChordSystem}
import pl.agh.harmonytools.model.harmonicfunction.{Delay, FunctionNames}
import pl.agh.harmonytools.model.key.{Key, Mode}
import pl.agh.harmonytools.model.scale.ScaleDegree
import pl.agh.harmonytools.model.util.ChordComponentManager

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

case class BeginningOfDefinition(key: String, metre: Meter)

object Tokens {
  final val colon              = ":"
  final val semicolon          = ";"
  final val dash               = "-"
  final val comma              = ","
  final val openCurlyBracket   = "{"
  final val closeCurlyBracket  = "}"
  final val openNormalBracket  = "("
  final val closeNormalBracket = ")"
  final val openSquareBracket  = "["
  final val closeSquareBracket = "]"
  final val eoi                = """\z""".r
  final val degree             = "degree"
  final val revolution         = "revolution"
  final val system             = "system"
  final val position           = "position"
  final val extra              = "extra"
  final val omit               = "omit"
  final val delay              = "delay"
  final val isRelatedBackwards = "isRelatedBackwards"
  final val down               = "down"
  final val newline            = "\n"
  final val meterBar           = "/"
  final val fieldsDelimiter    = "/"
  final val minorMode          = "o"
  final val tonicSymbol        = TONIC.name
  final val subdominantSymbol  = SUBDOMINANT.name
  final val dominantSymbol     = DOMINANT.name
  final val closeSystem        = "close"
  final val openSystem         = "open"
}

sealed trait ParserModel
case class Position(string: String)           extends ParserModel
case class Revolution(string: String)         extends ParserModel
case class IsRelatedBackwards(value: Boolean) extends ParserModel
case class IsDown(value: Boolean)             extends ParserModel
case class Extra(stringList: List[String])    extends ParserModel
case class Omit(stringList: List[String])     extends ParserModel
case class Delays(value: List[Delay])         extends ParserModel

class HarmonicsParser extends RegexParsers {
  override val whiteSpace: Regex = """[ \t\r]+""".r

  def key: Parser[Key]                 = """C#|c#|Cb|Db|d#|Eb|eb|F#|f#|Gb|g#|ab|Ab|a#|Bb|bb|[CcDdEeFfGgAaBb]""".r ^^ { key => Key(key) }
  def number: Parser[Int]              = """[1-9]\d*""".r ^^ { _.toInt }
  private val separator                = Tokens.eoi | Tokens.newline
  def meter: Parser[Meter]             = number ~ Tokens.meterBar ~ number ^^ { case n1 ~ b ~ n2 => Meter(n1, n2) }
  def alterationSymbol: Parser[String] = "<" | ">" | "<<" | ">>" ^^ { _.toString }

  def chordComponent1: Parser[String] =
    number ~ opt(alterationSymbol) ^^ {
      case n ~ Some(as) => n.toString + as
      case n ~ _        => n.toString
    }
  def chordComponent2: Parser[String] = alterationSymbol ~ number ^^ { case as ~ n => as + n.toString }
  def chordComponent: Parser[String]  = chordComponent1 | chordComponent2 ^^ { _.toString }

  def degree: Parser[Int] = """[1-7]""".r ^^ { _.toInt }

  def degreeDef: Parser[Degree] =
    Tokens.degree ~ Tokens.colon ~ degree ^^ { case d ~ c ~ value => ScaleDegree.fromValue(value) }

  def positionDef: Parser[Position] =
    Tokens.position ~ Tokens.colon ~ chordComponent ^^ { case p ~ c ~ cc => Position(cc) }

  def revolutionDef: Parser[Revolution] =
    Tokens.revolution ~ Tokens.colon ~ chordComponent ^^ { case r ~ c ~ cc => Revolution(cc) }

  def downDef: Parser[IsDown] =
    Tokens.down ^^ { x => IsDown(true) }

  def isRelatedBackwardsDef: Parser[IsRelatedBackwards] =
    Tokens.isRelatedBackwards ^^ { x => IsRelatedBackwards(true) }

  def extraDef: Parser[Extra] =
    Tokens.extra ~ Tokens.colon ~ chordComponent ~ rep(Tokens.comma ~ chordComponent) ^^ {
      case e ~ c ~ cc ~ rep => Extra(List(cc).appendedAll(rep.map(x => x._2)))
    }

  def omitDef: Parser[Omit] =
    Tokens.omit ~ Tokens.colon ~ chordComponent ~ rep(Tokens.comma ~ chordComponent) ^^ {
      case e ~ c ~ cc ~ rep => Omit(List(cc).appendedAll(rep.map(x => x._2)))
    }

  def delayDef: Parser[Delays] =
    Tokens.delay ~ Tokens.colon ~ chordComponent ~ Tokens.dash ~ chordComponent ~ rep(
      Tokens.comma ~ chordComponent ~ Tokens.dash ~ chordComponent
    ) ^^ {
      case e ~ c ~ cc1 ~ d ~ cc2 ~ rep =>
        Delays(List((cc1, cc2)).appendedAll(rep.map(x => (x._1._1._2, x._2))).map(Delay(_)))
    }

  def systemName: Parser[String] = Tokens.closeSystem | Tokens.openSystem ^^ { _.toString }

  def systemDef: Parser[System] =
    Tokens.system ~ Tokens.colon ~ systemName ^^ { case s ~ c ~ n => ChordSystem.fromString(n) }

  def harmonicFunctionNameDef: Parser[BaseFunction] =
    (Tokens.tonicSymbol | Tokens.subdominantSymbol | Tokens.dominantSymbol) ^^ (x => FunctionNames.fromName(x))

  def modeDef: Parser[BaseMode] = Tokens.minorMode ^^ (_ => Mode.MINOR)

  def harmonicFunctionContent: Parser[Any] =
    systemDef | delayDef | omitDef | extraDef | isRelatedBackwardsDef | downDef | revolutionDef | positionDef | degreeDef ^^ {
      s => s
    }

  def harmonicFunctionContentDef: Parser[HarmonicFunctionParserBuilder] =
    opt(harmonicFunctionContent ~ rep(Tokens.fieldsDelimiter ~ harmonicFunctionContent)) ^^ { s =>
      val builder = new HarmonicFunctionParserBuilder
      s match {
        case Some(value) =>
          value match {
            case hfContent ~ rep =>
              implicit def stringToChordComponent(x: String): ChordComponent =
                ChordComponentManager.chordComponentFromString(x)
              implicit def stringListToChordComponentList(xs: List[String]): List[ChordComponent] =
                xs.map(stringToChordComponent)

              rep.map(_._2).prepended(hfContent).foreach {
                case s: System              => builder.withSystem(s)
                case d: Delays              => builder.withDelay(d.value)
                case o: Omit                => builder.withOmit(o.stringList)
                case e: Extra               => builder.withExtra(e.stringList)
                case rb: IsRelatedBackwards => builder.withIsRelatedBackwards(rb.value)
                case d: IsDown              => builder.withIsDown(d.value)
                case r: Revolution          => builder.withRevolution(r.string)
                case p: Position            => builder.withPosition(p.string)
                case d: Degree              => builder.withDegree(d)
              }
              builder
          }
        case None => builder
      }

    }

  def harmonicFunctionDef: Parser[HarmonicFunctionParserBuilder] =
    harmonicFunctionNameDef ~ opt(
      modeDef
    ) ~ Tokens.openCurlyBracket ~ harmonicFunctionContentDef ~ Tokens.closeCurlyBracket ^^ {
      case name ~ Some(mode) ~ op ~ builder ~ cl =>
        builder.withBaseFunction(name)
        builder.withMode(mode)
        builder
      case name ~ None ~ op ~ builder ~ cl =>
        builder.withBaseFunction(name)
        builder
    }

  def measureDef: Parser[MeasureParserBuilder] =
    rep1(harmonicFunctionDef) ~ separator ^^ {
      case functions ~ separator => new MeasureParserBuilder(Some(functions))
    }

  def measuresDef: Parser[List[MeasureParserBuilder]] =
    rep1(measureDef) ^^ { measure => measure }

  def harmonicsExerciseDef: Parser[HarmonicsExercise] =
    key ~ Tokens.newline ~ meter ~ Tokens.newline ~ measuresDef ^^ {
      case key ~ nw1 ~ meter ~ nw2 ~ measures =>
        new HarmonicsExerciseParserBuilder(Some(key), Some(meter), Some(measures)).getHarmonicsExercise
    }

}

object TestParser extends HarmonicsParser {
  def main(args: Array[String]): Unit = {
    parse(harmonicsExerciseDef, """C#
        |4/4
        |T{degree:6} S{extra: 6} D{delay:4-3}""".stripMargin) match {
      case Success(matched, _) => println(matched)
      case Failure(msg, _)     => println(msg)
      case Error(msg, _)       => println(msg)
    }
  }
}
