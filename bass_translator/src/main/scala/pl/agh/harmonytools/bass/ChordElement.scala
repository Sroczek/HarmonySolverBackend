package pl.agh.harmonytools.bass

import Math.abs
import pl.agh.harmonytools.utils.Extensions.ExtendedInt

case class ChordElement(var notesNumbers: List[Int], var omit: List[Int], bassElement: FiguredBassElement) {
  private var primeNote: Option[Int] = None
  def setPrimeNote(baseNote: Int): Unit = primeNote = Some(baseNote)
  def getPrimeNote: Int = primeNote.getOrElse(sys.error("PrimeNote not defined"))

  def hasTwoNextThirds: Boolean = {
    if (notesNumbers.length < 3) false
    else {
      for (i <- notesNumbers.indices) {
        val n1 = notesNumbers(i) %% 7
        val n2 = notesNumbers((i + 1) %% notesNumbers.length) %% 7
        val n3 = notesNumbers((i + 2) %% notesNumbers.length) %% 7
        if ((abs(n2 - n1) == 2 || abs(n2 - n1) == 5) && (abs(n3 - n2) == 2 || abs(n3 - n2) == 5)) return true
      }
      false
    }
  }

  def addNextNote(): Unit = {
    for (i <- notesNumbers.dropRight(1).indices) {
      if (notesNumbers(i + 1) - notesNumbers(i) >= 4) {
        var tmp = List.empty[Int]
        for (j <- notesNumbers.indices) {
          tmp = tmp :+ notesNumbers(j)
          if (j == i) tmp = tmp :+ (notesNumbers(j) + 2)
        }
        notesNumbers = tmp
        if (notesNumbers.length >= 5) {
          omit = omit :+ ((notesNumbers.last %% 7) + 1)
        }
        return
      }
    }
    notesNumbers = notesNumbers :+ (notesNumbers.last + 2)
    if (notesNumbers.length >= 5) {
      omit = omit :+ (notesNumbers.last %% 7) + 1
    }
  }

  def completeUntilTwoNextThirds(): Unit = {
    while (!hasTwoNextThirds) {
      addNextNote()
    }
  }

  def findPrime(): Unit = {
    val scaleNotes = notesNumbers.map(_ %% 7)

    for (i <- scaleNotes.indices) {
      var note = scaleNotes(i)
      while (scaleNotes.contains((note - 2) %% 7)) {
        note = (note - 2) %% 7
      }
      if (scaleNotes.contains((note + 2) %% 7) && scaleNotes.contains((note + 4) %% 7)) {
        setPrimeNote(note)
        return
      }
    }

    primeNote match {
      case Some(_) =>
      case None => setPrimeNote(scaleNotes.head)
    }
  }

  def getSortedSymbols: List[Int] = bassElement.symbols.map(_.component).sortWith((s1, s2) => s1 < s2)

  def bassSymbolsHasGivenNumber(number: Int): Boolean = {
    !bassElement.symbols.forall(_.component != number)
  }
}
