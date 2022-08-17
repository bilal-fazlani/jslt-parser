package com.bilalfazlani.jslt.parsing

import com.bilalfazlani.jslt.parsing.JPrimitive.*
import com.bilalfazlani.jslt.parsing.Jslt.*
import zio.Chunk
import zio.test.*
import zio.test.Assertion.*

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
          equalTo(("key_name-1", JValue(value = JString(value = "value"))))
        )
      )
    },
    test("key value test without whitespace") {
      val input = """"key_name-2":"value""""
      val result = JsltSyntax.keyValueSyntax.parseString(input)
      assert(result)(
        isRight(
          equalTo(("key_name-2", JValue(value = JString(value = "value"))))
        )
      )
    },
    test("key value test for boolean value") {
      val input = """"key_name-3" : true""""
      val result = JsltSyntax.keyValueSyntax.parseString(input)
      assert(result)(
        isRight(
          equalTo(("key_name-3", JValue(value = JBoolean(value = true))))
        )
      )
    }
  )
}
