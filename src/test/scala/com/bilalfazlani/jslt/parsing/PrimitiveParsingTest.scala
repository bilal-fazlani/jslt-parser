package com.bilalfazlani.jslt.parsing

import com.bilalfazlani.jslt.parsing.models.Jslt
import com.bilalfazlani.jslt.parsing.models.Jslt.JValue._
import zio.test.Assertion._
import zio.test._

object PrimitiveParsingTest extends ZIOSpecDefault {
  def spec = suite("PrimitiveParsingTest")(
    test("parse integer") {
      val actual = Jslt.parse("123")
      assert(actual)(isRight(equalTo(JInteger(123))))
    },
    test("parse double") {
      val actual = Jslt.parse("123.456")
      assert(actual)(isRight(equalTo(JDouble(123.456))))
    },
    test("parse boolean true") {
      val actual = Jslt.parse("true")
      assert(actual)(isRight(equalTo(JBoolean(true))))
    },
    test("parse boolean false") {
      val actual = Jslt.parse("false")
      assert(actual)(isRight(equalTo(JBoolean(false))))
    },
    test("parse string") {
      val actual = Jslt.parse("\"hello\"")
      assert(actual)(isRight(equalTo(JString("hello"))))
    },
    test("parse string with white space") {
      val actual = Jslt.parse("\"hello world\"")
      assert(actual)(isRight(equalTo(JString("hello world"))))
    },
    test("parse string with special characters") {
      val actual = Jslt.parse("\"hello - _ @ world\"")
      assert(actual)(isRight(equalTo(JString("hello - _ @ world"))))
    },
    test("parse empty string"){
      val actual = Jslt.parse("\"\"")
      assert(actual)(isRight(equalTo(JString(""))))
    },
    test("parse whitespace") {
      val actual = Jslt.parse("\" \"")
      assert(actual)(isRight(equalTo(JString(" "))))
    },
    test("parse more whitespace") {
      val actual = Jslt.parse("\"  \"")
      assert(actual)(isRight(equalTo(JString("  "))))
    }
  )
}
