package pl.agh.harmonytools.harmonics.exercise

case class Meter(nominator: Int, denominator: Int) {
  def isPowerOf2(x: Int): Boolean = {
    x % 2 match {
      case 1 =>
        if (x == 1) true
        else false
      case 0 => isPowerOf2(x / 2)
    }
  }
  require(nominator > 0 && denominator > 0 && isPowerOf2(denominator), "Meter denominator should be power of 2")
}
