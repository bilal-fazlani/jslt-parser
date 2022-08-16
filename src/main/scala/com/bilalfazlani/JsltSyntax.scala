package com.bilalfazlani

import zio.Chunk
import zio.parser.*
import zio.parser.Syntax
import zio.parser.Syntax.*

object JsltSyntax {
  def literal(lit: String) = string(lit, ())

  val optionalWhitespace = whitespace.repeat0.unit(Chunk.empty)

  val comma = literal(",")
  val colon = literal(":")

  extension [In, Value](syntax: Syntax[_, In, _, Value]) {
    def quoted = syntax.surroundedBy(literal("\""))

    def curly = literal("{")
      ~ optionalWhitespace
      ~ syntax
      ~ optionalWhitespace
      ~ literal("}")
  }

  val keySyntax = alphaNumeric.repeat
    .transform(_.mkString, Chunk.fromIterable)
    .quoted
    .named("json key")
    .repeatWithSep(
      optionalWhitespace ~ comma ~ optionalWhitespace
    )

  val keyValueSyntax = keySyntax
    ~ optionalWhitespace
    ~ colon
    ~ optionalWhitespace
    ~ alphaNumeric.repeat
      .transform(_.mkString, Chunk.fromIterable)
}
