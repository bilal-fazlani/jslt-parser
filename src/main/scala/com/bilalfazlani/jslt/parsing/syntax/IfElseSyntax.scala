package com.bilalfazlani.jslt.parsing.syntax

import com.bilalfazlani.jslt.parsing.models.BooleanExpression
import com.bilalfazlani.jslt.parsing.models.BooleanExpression._
import com.bilalfazlani.jslt.parsing.models.Jslt.JIf
import zio.parser.{StringErrSyntaxOps, Syntax, SyntaxOps}
import zio.Chunk
import zio.parser.Syntax._

trait IfElseSyntax {
  this: JsltSyntax =>

  private[parsing] lazy val extractorSyntax =
    (literal("boolean").unit("boolean")
      ~ jPathSyntax.paren)
      .of[BooleanExtractor] ?? "boolean converter"

  private[parsing] lazy val jPathExpression =
    (jPathSyntax ~ end).of[JPathExpression] ?? "jpath expression"

  private[parsing] lazy val notExpression: Syntax[String, Char, Char, Not] =
    (literal("not").unit("not")
      ~ booleanExpression.paren)
      .of[Not] ?? "'not' expression"

  private[parsing] lazy val and =
    requiredWhitespace ~ literal("and").unit(" and ") ~ requiredWhitespace

  private[parsing] lazy val or =
    requiredWhitespace ~ literal("or").unit(" or ") ~ requiredWhitespace

  // and expression
  private[parsing] lazy val andExpression =
    (booleanExpression 
    ~ and 
    ~ booleanExpression
    )
      .of[And] ?? "'and' expression"

  // or expression
  private[parsing] lazy val orExpression =
    (booleanExpression ~ or ~ booleanExpression)
      .of[Or] ?? "'or' expression"

  private[parsing] lazy val booleanExpression
      : Syntax[String, Char, Char, BooleanExpression] =
    (
      extractorSyntax.widen[BooleanExpression] |
        jPathExpression.widen[BooleanExpression] |
        notExpression.widen[BooleanExpression] |
        andExpression.widen[BooleanExpression] |
        orExpression.widen[BooleanExpression]
    ) ?? "boolean expression"

  private[parsing] lazy val ifSyntax
      : Syntax[String, Char, Char, BooleanExpression] = (
    literal("if").unit("if")
      ~ optionalWhitespace ~
      booleanExpression.widen[BooleanExpression].paren
  ) ?? "if"

  private[parsing] lazy val elseSyntax = (
    literal("else").unit("else")
      ~ requiredWhitespace
      ~ jsltSyntax
  ) ?? "else"

  lazy val jIfElseSyntax: Syntax[String, Char, Char, JIf] =
    (ifSyntax ~ requiredWhitespace ~ jsltSyntax ~ requiredWhitespace ~ elseSyntax.optional)
      .of[JIf] ?? "if else"
}
