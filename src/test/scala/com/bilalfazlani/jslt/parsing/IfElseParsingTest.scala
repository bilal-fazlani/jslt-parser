package com.bilalfazlani.jslt.parsing

import com.bilalfazlani.jslt.parsing.models.{BooleanExpression, Jslt, JsltNode}
import com.bilalfazlani.jslt.parsing.models.Jslt.JValue._
import com.bilalfazlani.jslt.parsing.models.Jslt._
import zio.Chunk
import zio.test.Assertion._
import zio.test._

object IfElseParsingTest extends ZIOSpecDefault {
  def spec = suite("IfElseParsingTest")(
    test("parse boolean extractor top level if else expression") {
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
    test("parse jpath top level if else expression") {
      val input =
        """if (.details.marriage)
          |  "yes"
          |else
          |  "no"
          |""".stripMargin
      val expected = JIf(
        BooleanExpression.JPathExpression(JPath(JsltNode("details"), JsltNode("marriage"))),
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
