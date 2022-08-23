package com.bilalfazlani.jslt.parsing.syntax

import com.bilalfazlani.jslt.parsing.models.BooleanExpression.{BooleanExtractor, JPathExpression}
import com.bilalfazlani.jslt.parsing.models.Jslt.JIf
import com.bilalfazlani.jslt.parsing.models.BooleanExpression
import zio.parser.Syntax

trait IfElseSyntax {
  this: JsltSyntax =>

  private lazy val extractorSyntax = (literal("boolean").unit("boolean")
    ~ jPathSyntax.paren
    )
    .transform(
      path => BooleanExtractor(path),
      (x: BooleanExtractor) => x.path
    )

  private lazy val jPathExpressionSyntax = jPathSyntax.transform(
    path => JPathExpression(path),
    (x: JPathExpression) => x.path
  )

  private lazy val booleanExpression =
    extractorSyntax.widen[BooleanExpression] |
      jPathExpressionSyntax.widen[BooleanExpression]

  private lazy val ifSyntax = (
    literal("if").unit("if")
      ~ optionalWhitespace
      ~ literal("(").unit("(")
      ~ optionalWhitespace
      ~ booleanExpression
      ~ optionalWhitespace
      ~ literal(")").unit(")")
    )

  private lazy val elseSyntax = (
    literal("else").unit("else")
      ~ optionalWhitespace
      ~ jsltSyntax
    )

  lazy val jIfElseSyntax: Syntax[String, Char, Char, JIf] =
    (ifSyntax ~ optionalWhitespace ~ jsltSyntax ~ optionalWhitespace ~ elseSyntax.optional)
      .transform(
        { case (ifCond, thenExpression, elseExpression) =>
          JIf(ifCond, thenExpression, elseExpression)
        },
        (ifElse: JIf) => (ifElse.condition, ifElse.ifTrue, ifElse.ifFalse)
      )
}
