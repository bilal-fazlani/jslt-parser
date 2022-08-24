package com.bilalfazlani.jslt.parsing.syntax

import com.bilalfazlani.jslt.parsing.models.Jslt.{JArray, JObject}
import com.bilalfazlani.jslt.parsing.models.Jslt
import zio.Chunk
import zio.parser.Syntax
import zio.parser.SyntaxOps
import zio.parser.StringErrSyntaxOps

trait JsltSyntax extends PrimitiveSyntax with JPathSyntax with IfElseSyntax with MethodCallSyntax {

  lazy val jArraySyntax: Syntax[String, Char, Char, JArray] =
    jsltSyntax
      .separatedBy(comma)
      .withTrailingComma
      .array
      .of[JArray] ?? "array"

  lazy val jObjectSyntax: Syntax[String, Char, Char, JObject] =
    keyValueSyntax
      .separatedBy(comma)
      .withTrailingComma
      .curly
      .transform(
        items => JObject(items.toMap),
        (obj: JObject) => Chunk.fromIterable(obj.items)
      ) ?? "object"

  lazy val jsltSyntax: Syntax[String, Char, Char, Jslt] =
    (jArraySyntax.widen[Jslt] |
      jObjectSyntax.widen[Jslt] |
      jPrimitiveSyntax.widen[Jslt] |
      jPathSyntax.widen[Jslt] |
      jIfElseSyntax.widen[Jslt] |
      jMethodCallSyntax.widen[Jslt]).optionalParen ?? "jslt"

  lazy val keySyntax: Syntax[String, Char, Char, String] =
    anyStringCustom.quoted.named("key")

  lazy val keyValueSyntax: Syntax[String, Char, Char, (String, Jslt)] = (keySyntax
    ~ optionalWhitespace
    ~ colon
    ~ optionalWhitespace
    ~ jsltSyntax) ?? "key value"
}
