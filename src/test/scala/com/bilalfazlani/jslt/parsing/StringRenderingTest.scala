package com.bilalfazlani.jslt.parsing

import com.bilalfazlani.jslt.parsing.Jslt._
import zio.Chunk
import zio.test.Assertion._
import zio.test.TestAspect.ignore
import zio.test._

object StringRenderingTest extends ZIOSpecDefault with PrimitiveSyntax with JsltSyntax {
  def spec =
    suite("StringRenderingTest - Primitives")(
      test("render integer") {
        val input = JPrimitive.JInteger(1)
        val expected = "1"
        val result = jIntegerSyntax.printString(input)
        assert(result)(isRight(equalTo(expected)))
      },
      test("render string") {
        val input = JPrimitive.JString("hello")
        val expected = "\"hello\""
        val result = jStringSyntax.printString(input)
        assert(result)(isRight(equalTo(expected)))
      },
      test("render boolean true") {
        val input = JPrimitive.JBoolean(true)
        val expected = "true"
        val result = jBooleanSyntax.printString(input)
        assert(result)(isRight(equalTo(expected)))
      },
      test("render boolean false") {
        val input = JPrimitive.JBoolean(false)
        val expected = "false"
        val result = jBooleanSyntax.printString(input)
        assert(result)(isRight(equalTo(expected)))
      },
      test("render double") {
        val input = JPrimitive.JDouble(1.31)
        val expected = "1.31"
        val result = jDoubleSyntax.printString(input)
        assert(result)(isRight(equalTo(expected)))
      }
    ) @@ ignore +
      suite("StringRenderingTest - Arrays")(
        test("array of integers") {
          val input = JArray(
            Chunk(
              JPrimitive.JInteger(1),
              JPrimitive.JInteger(2),
              JPrimitive.JInteger(3)
            )
          )
          val expected = "[1, 2, 3]"
          val result = jArraySyntax.printString(input)
          assert(result)(isRight(equalTo(expected)))
        },
        test("array of strings") {
          val input = JArray(
            Chunk(
              JPrimitive.JString("hello"),
              JPrimitive.JString("world"),
              JPrimitive.JString("!")
            )
          )
          val expected = """["hello", "world", "!"]"""
          val result = jArraySyntax.printString(input)
          assert(result)(isRight(equalTo(expected)))
        }
      ) @@ ignore
}
