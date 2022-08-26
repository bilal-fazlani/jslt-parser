package com.bilalfazlani.jslt.parsing

import com.bilalfazlani.jslt.parsing.models.Jslt._
import com.bilalfazlani.jslt.parsing.syntax.{JsltSyntax, LiteralSyntax}
import zio.Chunk
import zio.test.Assertion._
import zio.test.TestAspect.ignore
import zio.test._

object StringRenderingTest extends ZIOSpecDefault with LiteralSyntax with JsltSyntax {
  def spec =
    suite("StringRenderingTest - Primitives")(
      test("render integer") {
        val input = JLiteral.JInteger(1)
        val expected = "1"
        val result = jIntegerSyntax.printString(input)
        assert(result)(isRight(equalTo(expected)))
      },
      test("render string") {
        val input = JLiteral.JString("hello")
        val expected = "\"hello\""
        val result = jStringSyntax.printString(input)
        assert(result)(isRight(equalTo(expected)))
      },
      test("render boolean true") {
        val input = JLiteral.JBoolean(true)
        val expected = "true"
        val result = jBooleanSyntax.printString(input)
        assert(result)(isRight(equalTo(expected)))
      },
      test("render boolean false") {
        val input = JLiteral.JBoolean(false)
        val expected = "false"
        val result = jBooleanSyntax.printString(input)
        assert(result)(isRight(equalTo(expected)))
      },
      test("render double") {
        val input = JLiteral.JDouble(1.31)
        val expected = "1.31"
        val result = jDoubleSyntax.printString(input)
        assert(result)(isRight(equalTo(expected)))
      }
    ) @@ ignore +
      suite("StringRenderingTest - Arrays")(
        test("array of integers") {
          val input = JArray(
            Chunk(
              JLiteral.JInteger(1),
              JLiteral.JInteger(2),
              JLiteral.JInteger(3)
            )
          )
          val expected = "[1, 2, 3]"
          val result = jArraySyntax.printString(input)
          assert(result)(isRight(equalTo(expected)))
        },
        test("array of strings") {
          val input = JArray(
            Chunk(
              JLiteral.JString("hello"),
              JLiteral.JString("world"),
              JLiteral.JString("!")
            )
          )
          val expected = """["hello", "world", "!"]"""
          val result = jArraySyntax.printString(input)
          assert(result)(isRight(equalTo(expected)))
        }
      ) @@ ignore
}
