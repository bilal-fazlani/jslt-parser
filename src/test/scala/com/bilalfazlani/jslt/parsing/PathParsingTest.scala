package com.bilalfazlani.jslt.parsing

import com.bilalfazlani.jslt.parsing.JPrimitive.*
import com.bilalfazlani.jslt.parsing.Jslt.*
import zio.test.*
import zio.Chunk
import zio.test.Assertion.*
import zio.test.junit.JUnitRunnableSpec

class PathParsingTest extends JUnitRunnableSpec {
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
    }
  )
}
