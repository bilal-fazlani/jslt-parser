package com.bilalfazlani.jslt.parsing

import com.bilalfazlani.jslt.parsing.models.{Jslt, JsltNode}
import com.bilalfazlani.jslt.parsing.models.Jslt._
import zio.Chunk
import zio.test.Assertion._
import zio.test._

object PathParsingTest extends ZIOSpecDefault {
  def spec = suite("PathParsingTest")(
    test("parse path of length 1") {
      val actual = Jslt.parse(".foo")
      assert(actual)(isRight(equalTo(JPath(Chunk(JsltNode("foo"))))))
    },
    test("parse path of length 2") {
      val actual = Jslt.parse(".foo.bar")
      assert(actual)(
        isRight(equalTo(JPath(Chunk(JsltNode("foo"), JsltNode("bar")))))
      )
    },
    test("parse path of length 3") {
      val actual = Jslt.parse(".foo.bar.baz")
      assert(actual)(
        isRight(
          equalTo(
            JPath(Chunk(JsltNode("foo"), JsltNode("bar"), JsltNode("baz")))
          )
        )
      )
    }
  )
}
