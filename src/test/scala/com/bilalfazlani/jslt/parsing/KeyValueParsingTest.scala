package com.bilalfazlani.jslt.parsing

import com.bilalfazlani.jslt.parsing.Jslt.JPrimitive._
import zio.test.Assertion._
import zio.test._

object KeyValueParsingTest extends ZIOSpecDefault {
  def spec = suite("ObjectParsingTest")(
    test("key test") {
      val input = """"key_name""""
      val result = JsltSyntax.keySyntax.parseString(input)
      assert(result)(isRight(equalTo("key_name")))
    },
    test("key value test") {
      val input = """"key_name-1" : "value""""
      val result = JsltSyntax.keyValueSyntax.parseString(input)
      assert(result)(
        isRight(
          equalTo(("key_name-1", JString(value = "value")))
        )
      )
    },
    test("key value test without whitespace") {
      val input = """"key_name-2":"value""""
      val result = JsltSyntax.keyValueSyntax.parseString(input)
      assert(result)(
        isRight(
          equalTo(("key_name-2", JString(value = "value")))
        )
      )
    },
    test("key value test for boolean value") {
      val input = """"key_name-3" : true""""
      val result = JsltSyntax.keyValueSyntax.parseString(input)
      assert(result)(
        isRight(
          equalTo(("key_name-3", JBoolean(value = true)))
        )
      )
    }
  )
}
