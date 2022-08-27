package com.bilalfazlani.jslt.parsing

import com.bilalfazlani.jslt.parsing.models.BooleanExpression._
import com.bilalfazlani.jslt.parsing.models.Jslt.JLiteral._
import com.bilalfazlani.jslt.parsing.models.Jslt._
import com.bilalfazlani.jslt.parsing.models.JsltNode
import com.bilalfazlani.jslt.parsing.syntax.{
  BooleanExpressionSyntax,
  JsltSyntax
}
import zio.Chunk
import zio.test.Assertion._
import zio.test._

object BooleanExpressionParsingTest
    extends ZIOSpecDefault
    with JsltSyntax
    with BooleanExpressionSyntax {
  def spec = suite("BooleanExpressionParsingTest")(
    test("parse boolean converter") {
      val input =
        "boolean(.details.marriage)"
      val expected =
        MethodCall("boolean", JPath(JsltNode("details"), JsltNode("marriage")))
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
    test("parse not of literal") {
      val input = "not(true)"
      val expected =
        Not(BooleanLiteral(JBoolean(true)))
      val result = booleanExpression.parseString(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse not of path expression") {
      val input = "not(.details.marriage)"
      val expected =
        Not(JPathExpression(JPath(JsltNode("details"), JsltNode("marriage"))))
      val result = booleanExpression.parseString(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse is-object of path expression") {
      val input = "is-object(.details)"
      val expected = MethodCall("is-object", JPath(JsltNode("details")))
      val result = booleanExpression.parseString(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse and expression") {
      val input = ".details.marriage and false"
      val expected =
        And(
          JPathExpression(JPath(JsltNode("details"), JsltNode("marriage"))),
          BooleanLiteral(JBoolean(false))
        )
      val result = booleanExpression.parseString(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse combined and expression") {
      val input =
        ".details and true and .details.children"
      val expected =
        And(
          And(
            JPathExpression(JPath(JsltNode("details"))),
            BooleanLiteral(JBoolean(true))
          ),
          JPathExpression(JPath(JsltNode("details"), JsltNode("children")))
        )

      val result = booleanExpression.parseString(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse or expression") {
      val input = ".details.marriage or .details.children"
      val expected =
        Or(
          JPathExpression(JPath(JsltNode("details"), JsltNode("marriage"))),
          JPathExpression(JPath(JsltNode("details"), JsltNode("children")))
        )
      val result = booleanExpression.parseString(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse combined or expression") {
      val input =
        ".details.marriage or .details and .details.children)"
      val expected =
        And(
          Or(
            JPathExpression(JPath(JsltNode("details"), JsltNode("marriage"))),
            JPathExpression(JPath(JsltNode("details")))
          ),
          JPathExpression(JPath(JsltNode("details"), JsltNode("children")))
        )
      val result = booleanExpression.parseString(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse not of and expression") {
      val input = "not(.details.marriage and .details.children)"
      val expected =
        Not(
          And(
            JPathExpression(JPath(JsltNode("details"), JsltNode("marriage"))),
            JPathExpression(JPath(JsltNode("details"), JsltNode("children")))
          )
        )
      val result = booleanExpression.parseString(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse not of or expression") {
      val input = "not(.details.marriage or .details.children)"
      val expected = Not(
        Or(
          JPathExpression(JPath(JsltNode("details"), JsltNode("marriage"))),
          JPathExpression(JPath(JsltNode("details"), JsltNode("children")))
        )
      )
      val result = booleanExpression.parseString(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse or of not of and expression") {
      val input =
        "not(.details.marriage and .details.children) or .details.marriage"
      val expected =
        Or(
          Not(
            And(
              JPathExpression(JPath(JsltNode("details"), JsltNode("marriage"))),
              JPathExpression(JPath(JsltNode("details"), JsltNode("children")))
            )
          ),
          JPathExpression(JPath(JsltNode("details"), JsltNode("marriage")))
        )
      val result = booleanExpression.parseString(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse not of and of not expression") {
      val input = "not(not(.details.marriage) and not(.details.children))"
      val expected = Not(
        And(
          Not(
            JPathExpression(JPath(JsltNode("details"), JsltNode("marriage")))
          ),
          Not(JPathExpression(JPath(JsltNode("details"), JsltNode("children"))))
        )
      )
      val result = booleanExpression.parseString(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse not of or of converter expression") {
      val input =
        "not(boolean(.details.marriage) or boolean(.details.children))"
      val expected = Not(
        Or(
          MethodCall("boolean", JPath(JsltNode("details"), JsltNode("marriage"))),
          MethodCall("boolean", JPath(JsltNode("details"), JsltNode("children")))
        )
      )
      val result = booleanExpression.parseString(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse and of or of and of boolean literals") {
      val input = "true and false or false and true"
      val one =
        And(BooleanLiteral(JBoolean(true)), BooleanLiteral(JBoolean(false)))
      val two = Or(one, BooleanLiteral(JBoolean(false)))
      val expected = And(two, BooleanLiteral(JBoolean(true)))
      val result = booleanExpression.parseString(input)
      assert(result)(isRight(equalTo(expected)))
    }
  )
}
