package com.bilalfazlani.jslt.parsing.syntax

import com.bilalfazlani.jslt.parsing.models.BooleanExpression
import com.bilalfazlani.jslt.parsing.models.BooleanExpression._
import com.bilalfazlani.jslt.parsing.models.Jslt.JIf
import zio.parser.{StringErrSyntaxOps, Syntax, SyntaxOps}

trait IfElseSyntax {
  this: JsltSyntax =>

  private lazy val extractorSyntax = (literal("boolean").unit("boolean")
    ~ jPathSyntax.paren)
    .of[BooleanExtractor] ?? "boolean converter"

  private lazy val jPathExpressionSyntax = jPathSyntax.of[JPathExpression] ?? "jpath boolean expression"

  private lazy val notExpression: Syntax[String, Char, Char, Not] =
    (literal("not").unit("not")
      ~ booleanExpression.paren)
      .of[Not] ?? "'not' boolean expression"

  private lazy val and =
    requiredWhitespace ~ literal("and").unit(" and ") ~ requiredWhitespace

  private lazy val or =
    requiredWhitespace ~ literal("or").unit(" or ") ~ requiredWhitespace

  // and expression
  private lazy val andExpression =
    (booleanExpression ~ and ~ booleanExpression).of[And] ?? "'and' boolean expression"

  // or expression
  private lazy val orExpression =
    (booleanExpression ~ or ~ booleanExpression).of[Or]  ?? "'or' boolean expression"

  private lazy val booleanExpression
      : Syntax[String, Char, Char, BooleanExpression] =
    (
      extractorSyntax.widen[BooleanExpression] |
        notExpression.widen[BooleanExpression] |
        jPathExpressionSyntax.widen[BooleanExpression] |
        andExpression.widen[BooleanExpression] |
        orExpression.widen[BooleanExpression]
    ) ?? "single boolean expression"

  private lazy val ifSyntax: Syntax[String, Char, Char, BooleanExpression] = (
    literal("if").unit("if")
      ~ optionalWhitespace ~
      booleanExpression.widen[BooleanExpression].paren
  ) ?? "if"

  private lazy val elseSyntax = (
    literal("else").unit("else")
      ~ requiredWhitespace
      ~ jsltSyntax
  ) ?? "else"

  lazy val jIfElseSyntax: Syntax[String, Char, Char, JIf] =
    (ifSyntax ~ requiredWhitespace ~ jsltSyntax ~ requiredWhitespace ~ elseSyntax.optional)
      .of[JIf] ?? "if else"
}
