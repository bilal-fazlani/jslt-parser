package com.bilalfazlani.jslt.parsing

import com.bilalfazlani.jslt.parsing.Jslt.JPrimitive._
import com.bilalfazlani.jslt.parsing.Jslt._
import zio.Chunk
import zio.test.Assertion._
import zio.test._

object IfElseParsingTest extends ZIOSpecDefault {
  def spec = suite("IfElseParsingTest")(
    test("parse simple top level if else expression") {
      val input =
        """if (boolean(.details.marriage))
          |  "yes"
          |else
          |  "no"
          |""".stripMargin
      val expected = JIf(
        BooleanExpression.BooleanExtractor(JPath(JsltNode("details"), JsltNode("marriage"))),
        JString("yes"),
        Some(JString("no"))
      )
      val result = Jslt.parse(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse simple if else expression in an object") {
      val input =
        """{ "name": "john", "isMarried": if (boolean(.details.marriage)) "yes" else "no" }"""
      val expected = JObject(
        Map(
          "name" -> JString("john"),
          "isMarried" -> JIf(
            BooleanExpression.BooleanExtractor(JPath(JsltNode("details"), JsltNode("marriage"))),
            JString("yes"),
            Some(JString("no")))
        )
      )
      val result = Jslt.parse(input)
      assert(result)(isRight(equalTo(expected)))
    }
  )
}