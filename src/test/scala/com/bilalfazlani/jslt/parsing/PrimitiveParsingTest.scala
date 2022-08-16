package com.bilalfazlani.jslt.parsing

import com.bilalfazlani.jslt.parsing.JPrimitive.*
import com.bilalfazlani.jslt.parsing.Jslt.*
import zio.test.*
import zio.test.Assertion.*

object PrimitiveParsingTest extends ZIOSpecDefault {
  def spec = suite("PrimitiveParsingTest")(
    test("parse integer") {
      val actual = Jslt.parse("123")
      assert(actual)(isRight(equalTo(JValue(JInteger(123)))))
    },
    test("parse double") {
      val actual = Jslt.parse("123.456")
      assert(actual)(isRight(equalTo(JValue(JDouble(123.456)))))
    },
    test("parse boolean true") {
      val actual = Jslt.parse("true")
      assert(actual)(isRight(equalTo(JValue(JBoolean(true)))))
    },
    test("parse boolean false") {
      val actual = Jslt.parse("false")
      assert(actual)(isRight(equalTo(JValue(JBoolean(false)))))
    },
    test("parse string") {
      val actual = Jslt.parse("\"hello\"")
      assert(actual)(isRight(equalTo(JValue(JString("hello")))))
    },
    test("parse string with white space") {
      val actual = Jslt.parse("\"hello world\"")
      assert(actual)(isRight(equalTo(JValue(JString("hello world")))))
    }
  )
}
