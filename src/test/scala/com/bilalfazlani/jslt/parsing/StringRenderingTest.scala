package com.bilalfazlani.jslt.parsing

import com.bilalfazlani.jslt.parsing.Jslt._
import zio.test._
import zio.test.Assertion._
import JsltSyntax._

object StringRenderingTest extends ZIOSpecDefault {
  def spec =
    suite("StringRenderingTest - Primitives")(
      test("render integer") {
        val input = JValue(JPrimitive.JInteger(1))
        val expected = "1"
        val result = jIntegerSyntax.printString(input)
        assert(result)(isRight(equalTo(expected)))
      },
      test("render string") {
        val input = JValue(JPrimitive.JString("hello"))
        val expected = "\"hello\""
        val result = jStringSyntax.printString(input)
        assert(result)(isRight(equalTo(expected)))
      },
      test("render boolean true") {
        val input = JValue(JPrimitive.JBoolean(true))
        val expected = "true"
        val result = jBooleanSyntax.printString(input)
        assert(result)(isRight(equalTo(expected)))
      },
      test("render boolean false") {
        val input = JValue(JPrimitive.JBoolean(false))
        val expected = "false"
        val result = jBooleanSyntax.printString(input)
        assert(result)(isRight(equalTo(expected)))
      },
      test("render double") {
        val input = JValue(JPrimitive.JDouble(1.31))
        val expected = "1.31"
        val result = jDoubleSyntax.printString(input)
        assert(result)(isRight(equalTo(expected)))
      }
    )
}
