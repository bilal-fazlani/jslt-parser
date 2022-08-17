package com.bilalfazlani.jslt.parsing

import com.bilalfazlani.jslt.parsing.JPrimitive._
import com.bilalfazlani.jslt.parsing.Jslt._
import zio.test._
import zio.test.Assertion._

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
    },
    test("parse string with special characters") {
      val actual = Jslt.parse("\"hello - _ @ world\"")
      assert(actual)(isRight(equalTo(JValue(JString("hello - _ @ world")))))
    }
  )
}
