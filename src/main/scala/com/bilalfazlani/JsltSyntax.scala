package com.bilalfazlani

import zio.Chunk
import zio.parser.*
import zio.parser.Syntax
import zio.parser.Syntax.*
import Jslt.*

object JsltSyntax {
  def literal(lit: String): Syntax[String, Char, Char, String] =
    string(lit, lit)

  val optionalWhitespace: Syntax[String, Char, Char, Unit] =
    whitespace.repeat0.unit(Chunk.empty)

  val alphanumericString: Syntax[String, Char, Char, String] =
    alphaNumeric.repeat
      .transform(_.mkString, Chunk.fromIterable)

  val anyStringCustom: Syntax[String, Char, Char, String] =
    Syntax.notChar('"').repeat.transform(_.mkString, Chunk.fromIterable)

  val comma: Syntax[String, Char, Char, Unit] = literal(",").unit(", ")

  val colon: Syntax[String, Char, Char, Unit] = literal(":").unit(": ")

  extension [Err, In, Out, Value](syntax: Syntax[Err, In, Out, Value]) {
    def quoted = syntax.between(
      literal("\"").unit("\""),
      literal("\"").unit("\"")
    )

    def curly = literal("{").unit("{")
      ~ optionalWhitespace
      ~ syntax
      ~ optionalWhitespace
      ~ literal("}").unit("}")
  }

  val jStringSyntax: Syntax[Any, Char, Any, Jslt] =
    anyStringCustom.quoted
      .transform(
        JString.apply,
        (jslt: Jslt) => jslt.asInstanceOf[JString].value
      )

  val jBooleanSyntax: Syntax[String, Char, Char, Jslt] = Syntax
    .oneOf(literal("true"), literal("false"))
    .transform[Jslt](
      x => JBoolean(x.toBoolean),
      (jslt: Jslt) => jslt.asInstanceOf[JBoolean].value.toString
    )

  def jsltSyntax: Syntax[Any, Char, Any, Jslt] =
    jBooleanSyntax <> jStringSyntax

  val keySyntax: Syntax[Any, Char, Any, String] =
    alphanumericString.quoted
      .named("json key")

  val keyValueSyntax: Syntax[Any, Char, Any, (String, Jslt)] = keySyntax
    ~ optionalWhitespace
    ~ colon
    ~ optionalWhitespace
    ~ jsltSyntax

  val jObjectSyntax: Syntax[Any, Char, Any, JObject] =
    keyValueSyntax
      .repeatWithSep(optionalWhitespace ~ comma ~ optionalWhitespace)
      .curly
      .transform(
        items => JObject(items.toMap),
        (obj: JObject) => Chunk.fromIterable(obj.items)
      )
}
