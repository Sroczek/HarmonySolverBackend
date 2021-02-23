package example

import org.scalatest.{FunSuite, Matchers}

class ExampleTest extends FunSuite with Matchers {
  test("should transform to lower case") {
    val lower = "aaa"
    Example.toLowerCase("AAA") shouldBe lower
    Example.toLowerCase("AaA") shouldBe lower
    Example.toLowerCase(lower) shouldBe lower
  }
}
