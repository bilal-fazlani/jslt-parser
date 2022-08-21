package com.bilalfazlani.jslt.parsing

import com.bilalfazlani.jslt.parsing.Jslt._
import zio.Chunk
import zio.parser.{Syntax, _}

trait JsltSyntax extends PrimitiveSyntax with JPathSyntax {

  def jArraySyntax: Syntax[String, Char, Char, JArray] =
    (
      jPrimitiveSyntax.widen[Jslt] |
        jObjectSyntax.widen[Jslt] |
        jPathSyntax.widen[Jslt]
      )
      .separatedBy(comma)
      .withTrailingComma
      .array
      .transform(
        items => JArray(items),
        (arr: JArray) => arr.items
      )

  def jObjectSyntax: Syntax[String, Char, Char, JObject] =
    keyValueSyntax
      .separatedBy(comma)
      .withTrailingComma
      .curly
      .transform(
        items => JObject(items.toMap),
        (obj: JObject) => Chunk.fromIterable(obj.items)
      )

  def jsltSyntax: Syntax[String, Char, Char, Jslt] =
    jArraySyntax.widen[Jslt] |
      jObjectSyntax.widen[Jslt] |
      jPrimitiveSyntax.widen[Jslt] |
      jPathSyntax.widen[Jslt] |
      jIfElseSyntax.widen[Jslt]

  val keySyntax: Syntax[String, Char, Char, String] =
    anyStringCustom.quoted
      .named("json key")

  val keyValueSyntax: Syntax[String, Char, Char, (String, Jslt)] = (keySyntax
    ~ optionalWhitespace
    ~ colon
    ~ optionalWhitespace
    ~ jsltSyntax)

  val booleanExpression: Syntax[String, Char, Char, BooleanExpression] = ???

  val ifSyntax = (
    literal("if").unit("if")
      ~ optionalWhitespace
      ~ literal("(").unit("(")
      ~ optionalWhitespace
      ~ booleanExpression
      ~ optionalWhitespace
      ~ literal(")").unit(")")
    )

  val elseSyntax = (
    literal("else").unit("else")
      ~ optionalWhitespace
      ~ jsltSyntax
    )

  def jIfElseSyntax =
    (ifSyntax ~ optionalWhitespace ~ jsltSyntax ~ optionalWhitespace ~ elseSyntax.optional)
      .transform(
        { case (ifCond, thenExpression, elseExpression) =>
          JIf(ifCond, thenExpression, elseExpression)
        },
        (ifElse: JIf) => (ifElse.condition, ifElse.ifTrue, ifElse.ifFalse)
      )
}



