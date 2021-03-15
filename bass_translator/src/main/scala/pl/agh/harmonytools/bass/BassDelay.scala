package pl.agh.harmonytools.bass

case class BassDelay(first: BassSymbol, second: BassSymbol) {
  def mapToChordComponentDelay(): ChordComponentDelay =
    ChordComponentDelay(first.mapToChordComponentSymbol(), second.mapToChordComponentSymbol())
}

case class ChordComponentDelay(first: ChordComponentSymbol, second: ChordComponentSymbol)
