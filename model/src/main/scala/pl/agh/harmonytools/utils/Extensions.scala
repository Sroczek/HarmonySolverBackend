package pl.agh.harmonytools.utils

object Extensions
{
  implicit class ExtendedInt (val i: Int) extends AnyVal {
    def %% (m: Int): Int = {val x = i % m; if (x < 0) x + m else x}
  }
}
