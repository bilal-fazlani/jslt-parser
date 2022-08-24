package com.bilalfazlani.jslt.parsing

import com.bilalfazlani.jslt.parsing.models.BooleanExpression._
import com.bilalfazlani.jslt.parsing.models.Jslt.JValue._
import com.bilalfazlani.jslt.parsing.models.Jslt._
import com.bilalfazlani.jslt.parsing.models.{Jslt, JsltNode}
import com.bilalfazlani.jslt.parsing.syntax.{IfElseSyntax, JsltSyntax}
import zio.test.Assertion._
import zio.test.TestAspect.ignore
import zio.test._
import zio.Chunk

//boolean extractor
//jpath expression
//not expression
//and expression

object BooleanExpressionParsing
    extends ZIOSpecDefault
    with JsltSyntax
    with IfElseSyntax {
  def spec = suite("BooleanExpressionParsing")(
    test("parse boolean extractor") {
      val input =
        "boolean(.details.marriage)"
      val expected =
        BooleanExtractor(JPath(JsltNode("details"), JsltNode("marriage")))
      val result = booleanExpression.parseString(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse jpath expression") {
      val input = ".details.marriage"
      val expected =
        JPathExpression(JPath(Chunk(JsltNode("details"), JsltNode("marriage"))))
      val result = booleanExpression.parseString(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse not expression") {
      val input = "not(boolean(.details.marriage))"
      val expected =
        Not(BooleanExtractor(JPath(JsltNode("details"), JsltNode("marriage"))))
      val result = booleanExpression.parseString(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse and expression") {
      val input = "boolean(.details.marriage) and .details.children"
      val expected =
        And(
          BooleanExtractor(JPath(JsltNode("details"), JsltNode("marriage"))),
          JPathExpression(JPath(JsltNode("details"), JsltNode("children")))
        )
      val result = booleanExpression.parseString(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse combined and expression") {
      val input =
        "boolean(.details.marriage) and (.details and .details.children)"
      val expected =
        And(
          BooleanExtractor(JPath(JsltNode("details"), JsltNode("marriage"))),
          And(
            JPathExpression(JPath(JsltNode("details"))),
            JPathExpression(JPath(JsltNode("details"), JsltNode("children")))
          )
        )

      val result = booleanExpression.parseString(input)
      assert(result)(isRight(equalTo(expected)))
    } @@ ignore,
    test("parse or expression") {
      val input = "boolean(.details.marriage) or .details.children"
      val expected =
        Or(
          BooleanExtractor(JPath(JsltNode("details"), JsltNode("marriage"))),
          JPathExpression(JPath(JsltNode("details"), JsltNode("children")))
        )
      val result = booleanExpression.parseString(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse combined or expression") {
      val input =
        "boolean(.details.marriage) or (.details and .details.children)"
      val expected =
        Or(
          BooleanExtractor(JPath(JsltNode("details"), JsltNode("marriage"))),
          And(
            JPathExpression(JPath(JsltNode("details"))),
            JPathExpression(JPath(JsltNode("details"), JsltNode("children")))
          )
        )
      val result = booleanExpression.parseString(input)
      assert(result)(isRight(equalTo(expected)))
    } @@ ignore,
    test("parse not of and expression"){
      val input = "not(boolean(.details.marriage) and .details.children)"
      val expected =
        Not(
          And(
            BooleanExtractor(JPath(JsltNode("details"), JsltNode("marriage"))),
            JPathExpression(JPath(JsltNode("details"), JsltNode("children")))
          )
        )
      val result = booleanExpression.parseString(input)
      assert(result)(isRight(equalTo(expected)))
    } @@ ignore,
    test("parse not of or expression"){
      val input = "not(boolean(.details.marriage) or not(.details.children))"
      val expected = Or(
            Not(BooleanExtractor(JPath(JsltNode("details"), JsltNode("marriage")))),
            Not(JPathExpression(JPath(JsltNode("details"), JsltNode("children"))))
        )
      val result = booleanExpression.parseString(input)
      assert(result)(isRight(equalTo(expected)))
    } @@ ignore
  )
}

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
        Not(
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
    } @@ ignore,
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
    } @@ ignore,
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
