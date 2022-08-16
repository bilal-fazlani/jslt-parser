package com.bilalfazlani

import zio.Chunk
import zio.parser.*
import zio.parser.Syntax
import zio.parser.Syntax.*

object JsltSyntax {
  def literal(lit: String): Syntax[String, Char, Char, String] =
    string(lit, lit)

  val optionalWhitespace: Syntax[String, Char, Char, Unit] =
    whitespace.repeat0.unit(Chunk.empty)

  val validString: Syntax[String, Char, Char, String] = alphaNumeric.repeat
    .transform(_.mkString, Chunk.fromIterable)

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

  val jsltSyntax: Syntax[String, Char, Char, Jslt] = ???

  val keySyntax =
    validString.quoted
      .named("json key")

  val keyValueSyntax = keySyntax
    ~ optionalWhitespace
    ~ colon
    ~ optionalWhitespace
    ~ jsltSyntax

  val jObjectSyntax = keyValueSyntax
    .repeatWithSep(comma)
    .curly
    .transform(
      items => Jslt.JObject(items.toMap),
      (obj: Jslt) => Chunk.fromIterable(obj.asInstanceOf[Jslt.JObject].items)
    )
}
