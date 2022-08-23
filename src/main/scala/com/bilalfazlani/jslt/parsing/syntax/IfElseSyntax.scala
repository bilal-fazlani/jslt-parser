package com.bilalfazlani.jslt.parsing.syntax

import com.bilalfazlani.jslt.parsing.models.BooleanExpression._
import com.bilalfazlani.jslt.parsing.models.Jslt.JIf
import com.bilalfazlani.jslt.parsing.models.BooleanExpression
import zio.parser.Syntax
import zio.parser.SyntaxOps
import zio.parser.StringErrSyntaxOps

trait IfElseSyntax {
  this: JsltSyntax =>

  private lazy val extractorSyntax = (literal("boolean").unit("boolean")
    ~ jPathSyntax.paren)
    .transform(
      path => BooleanExtractor(path),
      (x: BooleanExtractor) => x.path
    ) ?? "boolean converter"

  private lazy val jPathExpressionSyntax = jPathSyntax.transform(
    path => JPathExpression(path),
    (x: JPathExpression) => x.path
  ) ?? "jpath boolean expression"

  private lazy val notExpression: Syntax[String, Char, Char, Not] =
    (literal("not").unit("not")
      ~ booleanExpression.paren)
      .transform(
        expression => Not(expression),
        (x: Not) => x.expression
      ) ?? "'not' boolean expression"

  // and expression
  private lazy val andExpression =
    (booleanExpression ~ requiredWhitespace ~ literal("and").unit(" and ")
      ~ requiredWhitespace ~ booleanExpression)
      .transform(
        { case (left, right) => And(left, right) },
        (x: And) => (x.left, x.right)
      ) ?? "'and' boolean expression"

  // or expression
  private lazy val orExpression = (
    booleanExpression ~ requiredWhitespace ~ literal("or").unit(" or ")
      ~ requiredWhitespace ~ booleanExpression
  ).transform(
    { case (left, right) => Or(left, right) },
    (x: Or) => (x.left, x.right)
  ) ?? "'or' boolean expression"

  private lazy val booleanExpression
      : Syntax[String, Char, Char, BooleanExpression] =
    (
      extractorSyntax.widen[BooleanExpression] |
        andExpression.widen[BooleanExpression] |
        orExpression.widen[BooleanExpression] |
        notExpression.widen[BooleanExpression] |
        jPathExpressionSyntax.widen[BooleanExpression]
    ) ?? "boolean expression"

  private lazy val ifSyntax = (
    literal("if").unit("if")
      ~ optionalWhitespace
      ~ booleanExpression.paren
  ) ?? "if"

  private lazy val elseSyntax = (
    literal("else").unit("else")
      ~ optionalWhitespace
      ~ jsltSyntax
  ) ?? "else"

  lazy val jIfElseSyntax: Syntax[String, Char, Char, JIf] =
    (ifSyntax ~ optionalWhitespace ~ jsltSyntax ~ optionalWhitespace ~ elseSyntax.optional)
      .transform(
        { case (ifCond, thenExpression, elseExpression) =>
          JIf(ifCond, thenExpression, elseExpression)
        },
        (ifElse: JIf) => (ifElse.condition, ifElse.ifTrue, ifElse.ifFalse)
      ) ?? "if else"
}
