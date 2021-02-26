package pl.agh.harmonytools.model.chord

import scala.collection.immutable.HashMap

case class ChordComponent(
  chordComponentString: String,
  baseComponent: Int,
  semitonesNumber: Int,
  isDown: Boolean
) {
  override def toString: String = chordComponentString

  lazy val toXmlString: String =
    chordComponentString.replace("<", "&lt;").replace(">", "&gt;")
}

object ChordComponent {

  final val baseComponentPitch: HashMap[Int, Int] = HashMap(
    1 -> 0,
    2 -> 2,
    3 -> 4,
    4 -> 5,
    5 -> 7,
    6 -> 9,
    7 -> 10,
    8 -> 12,
    9 -> 14,
    10 -> 16,
    11 -> 17,
    12 -> 19,
    13 -> 21
  )

  def apply(chordComponentString: String, isDown: Boolean = false): ChordComponent = {
    val baseComponent   = {
      if (chordComponentString.head.isDigit) Integer.parseInt(chordComponentString.takeWhile(_.isDigit))
      else Integer.parseInt(chordComponentString.reverse.takeWhile(_.isDigit).reverse)
    }
    var semitonesNumber = baseComponentPitch.getOrElse(baseComponent, throw new IllegalArgumentException("Illegal baseComponent: " + baseComponent))
    semitonesNumber += chordComponentString.count(_ == '<')
    semitonesNumber -= chordComponentString.count(_ == '>')
    new ChordComponent(
      chordComponentString,
      baseComponent,
      semitonesNumber,
      isDown
    )
  }
}
