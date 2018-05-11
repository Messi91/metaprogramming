package com.mesfin.meta.annotations

import org.scalatest.{Matchers, WordSpec}

class MacrosTest extends WordSpec with Matchers {

  "Function after macro transformation" should {

    "cache function result" in new Context {
      testFunction("test", 1, 1)
      testFunction("test", 1, 1)
      testFunction("test", 1, 1)
      testFunction("test", 1, 1)

      callsCount shouldBe 1
    }

    "cache function result depends on parameters" in new Context {
      testFunction("test", 1, 1)
      testFunction("test1", 1, 1)
      testFunction("test", 2, 1)
      testFunction("test", 1, 3)

      callsCount shouldBe 4
    }
  }
}

trait Context {
  var callsCount = 0

  @Memoize
  def testFunction(a: String, b: Int, c: Long) = {
    callsCount += 1
    s"$a - $b - $c"
  }
}
