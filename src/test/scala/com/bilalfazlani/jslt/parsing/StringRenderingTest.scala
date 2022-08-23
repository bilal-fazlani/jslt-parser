package com.bilalfazlani.jslt.parsing

import com.bilalfazlani.jslt.parsing.models.Jslt._
import com.bilalfazlani.jslt.parsing.syntax.{JsltSyntax, PrimitiveSyntax}
import zio.Chunk
import zio.test.Assertion._
import zio.test.TestAspect.ignore
import zio.test._

object StringRenderingTest extends ZIOSpecDefault with PrimitiveSyntax with JsltSyntax {
  def spec =
    suite("StringRenderingTest - Primitives")(
      test("render integer") {
        val input = JValue.JInteger(1)
        val expected = "1"
        val result = jIntegerSyntax.printString(input)
        assert(result)(isRight(equalTo(expected)))
      },
      test("render string") {
        val input = JValue.JString("hello")
        val expected = "\"hello\""
        val result = jStringSyntax.printString(input)
        assert(result)(isRight(equalTo(expected)))
      },
      test("render boolean true") {
        val input = JValue.JBoolean(true)
        val expected = "true"
        val result = jBooleanSyntax.printString(input)
        assert(result)(isRight(equalTo(expected)))
      },
      test("render boolean false") {
        val input = JValue.JBoolean(false)
        val expected = "false"
        val result = jBooleanSyntax.printString(input)
        assert(result)(isRight(equalTo(expected)))
      },
      test("render double") {
        val input = JValue.JDouble(1.31)
        val expected = "1.31"
        val result = jDoubleSyntax.printString(input)
        assert(result)(isRight(equalTo(expected)))
      }
    ) @@ ignore +
      suite("StringRenderingTest - Arrays")(
        test("array of integers") {
          val input = JArray(
            Chunk(
              JValue.JInteger(1),
              JValue.JInteger(2),
              JValue.JInteger(3)
            )
          )
          val expected = "[1, 2, 3]"
          val result = jArraySyntax.printString(input)
          assert(result)(isRight(equalTo(expected)))
        },
        test("array of strings") {
          val input = JArray(
            Chunk(
              JValue.JString("hello"),
              JValue.JString("world"),
              JValue.JString("!")
            )
          )
          val expected = """["hello", "world", "!"]"""
          val result = jArraySyntax.printString(input)
          assert(result)(isRight(equalTo(expected)))
        }
      ) @@ ignore
}
