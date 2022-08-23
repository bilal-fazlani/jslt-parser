package com.bilalfazlani.jslt.parsing

import com.bilalfazlani.jslt.parsing.models.BooleanExpression.{And, BooleanExtractor, JPathExpression, Or}
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
        BooleanExtractor(
          JPath(JsltNode("details"), JsltNode("marriage"))
        ),
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
        JPathExpression(
          JPath(JsltNode("details"), JsltNode("marriage"))
        ),
        JString("yes"),
        Some(JString("no"))
      )
      val result = Jslt.parse(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test(
      "parse not top level if else expression with jpath expression inside"
    ) {
      val input =
        """if (not(.details.marriage))
          |  "single"
          |else
          |  "married"
          |""".stripMargin
      val expected = JIf(
        BooleanExpression.Not(
          JPathExpression(
            JPath(JsltNode("details"), JsltNode("marriage"))
          )
        ),
        JString("single"),
        Some(JString("married"))
      )
      val result = Jslt.parse(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse or boolean expression") {
      val input =
        """if (.details.marriage or boolean(.details.children))
            |  "yes"
            |else
            |  "no"
            |""".stripMargin
      val expected = JIf(
        Or(
          JPathExpression(
            JPath(JsltNode("details"), JsltNode("marriage"))
          ),
          BooleanExtractor(
            JPath(JsltNode("details"), JsltNode("children"))
          )
        ),
        JString("yes"),
        Some(JString("no"))
      )
      val result = Jslt.parse(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse and boolean expression") {
      val input =
        """if (.details.marriage and boolean(.details.children))
            |  "children"
            |else
            |  "no children"
            |""".stripMargin
      val expected = JIf(
        And(
          JPathExpression(
            JPath(JsltNode("details"), JsltNode("marriage"))
          ),
          BooleanExtractor(
            JPath(JsltNode("details"), JsltNode("children"))
          )
        ),
        JString("children"),
        Some(JString("no children"))
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
            BooleanExtractor(
              JPath(JsltNode("details"), JsltNode("marriage"))
            ),
            JString("yes"),
            Some(JString("no"))
          )
        )
      )
      val result = Jslt.parse(input)
      assert(result)(isRight(equalTo(expected)))
    }
  )
}
