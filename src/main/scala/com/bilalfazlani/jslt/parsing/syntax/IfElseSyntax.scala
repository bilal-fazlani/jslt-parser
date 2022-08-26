package com.bilalfazlani.jslt.parsing.syntax

import com.bilalfazlani.jslt.parsing.models.BooleanExpression
import com.bilalfazlani.jslt.parsing.models.BooleanExpression._
import com.bilalfazlani.jslt.parsing.models.Jslt.JIf
import zio.parser.{StringErrSyntaxOps, Syntax, SyntaxOps}
import zio.Chunk
import zio.parser.Syntax._

trait IfElseSyntax extends BooleanExpressionSyntax {
  this: JsltSyntax =>

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
