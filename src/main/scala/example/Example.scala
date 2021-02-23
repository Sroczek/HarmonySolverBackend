package example

object Example {
  def toLowerCase(s: String): String = {
    if (s.isEmpty) {
      ""
    } else {
      s.head.toLower + toLowerCase(s.tail)
    }
  }
}
