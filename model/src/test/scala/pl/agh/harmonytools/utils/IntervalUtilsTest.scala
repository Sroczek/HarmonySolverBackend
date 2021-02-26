package pl.agh.harmonytools.utils

import org.scalatest.{FunSuite, Matchers}
import pl.agh.harmonytools.model.key.Key
import pl.agh.harmonytools.model.key.Mode.{MAJOR, MINOR}
import pl.agh.harmonytools.model.scale.ScaleDegree

class IntervalUtilsTest extends FunSuite with Matchers {
  test("Get third mode") {
    IntervalUtils.getThirdMode(Key("C"), ScaleDegree.I) shouldBe MAJOR
    IntervalUtils.getThirdMode(Key("C"), ScaleDegree.II) shouldBe MINOR
    IntervalUtils.getThirdMode(Key("C"), ScaleDegree.III) shouldBe MINOR
    IntervalUtils.getThirdMode(Key("C"), ScaleDegree.IV) shouldBe MAJOR
    IntervalUtils.getThirdMode(Key("C"), ScaleDegree.V) shouldBe MAJOR
    IntervalUtils.getThirdMode(Key("C"), ScaleDegree.VI) shouldBe MINOR
    IntervalUtils.getThirdMode(Key("C"), ScaleDegree.VII) shouldBe MINOR
    IntervalUtils.getThirdMode(Key("c"), ScaleDegree.I) shouldBe MINOR
    IntervalUtils.getThirdMode(Key("c"), ScaleDegree.II) shouldBe MINOR
    IntervalUtils.getThirdMode(Key("c"), ScaleDegree.III) shouldBe MAJOR
    IntervalUtils.getThirdMode(Key("c"), ScaleDegree.IV) shouldBe MINOR
    IntervalUtils.getThirdMode(Key("c"), ScaleDegree.V) shouldBe MINOR
    IntervalUtils.getThirdMode(Key("c"), ScaleDegree.VI) shouldBe MAJOR
    IntervalUtils.getThirdMode(Key("c"), ScaleDegree.VII) shouldBe MAJOR
  }
}
