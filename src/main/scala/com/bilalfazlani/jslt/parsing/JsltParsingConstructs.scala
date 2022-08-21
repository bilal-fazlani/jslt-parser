package com.bilalfazlani.jslt.parsing

import zio.Chunk
import zio.parser.Syntax
import zio.parser.Syntax.{char, charIn, string, whitespace}

trait JsltParsingConstructs {
  def literal(lit: String): Syntax[String, Char, Char, String] =
    string(lit, lit)

  val optionalWhitespace: Syntax[String, Char, Char, Unit] =
    whitespace.repeat0.unit(Chunk.empty)

  val acceptableChars: Syntax[String, Char, Char, Char] = charIn("_-")

  val newLine = char('\n')

  val openParen = optionalWhitespace ~ literal("(").unit("(")
  val closeParen = optionalWhitespace ~ literal(")").unit(")")

  val anyStringCustom: Syntax[String, Char, Char, String] =
    Syntax
      .notChar('"')
      .repeat
      .transform(_.mkString, str => Chunk.fromIterable(str))

  val comma: Syntax[String, Char, Char, Unit] = literal(",").unit(", ")

  val colon: Syntax[String, Char, Char, Unit] = literal(":").unit(": ")

  implicit class SyntaxExtensions[Value](
                                          syntax: Syntax[String, Char, Char, Value]
                                        ) {
    def quoted: Syntax[String, Char, Char, Value] = syntax.between(
      literal("\"").unit("\""),
      literal("\"").unit("\"")
    )

    def separatedBy(separator: Syntax[String, Char, Char, Unit]) =
      syntax.repeatWithSep(
        optionalWhitespace ~ separator ~ optionalWhitespace
      )

    def withTrailingComma = syntax ~ comma.optional.unit(None)

    def curly = (literal("{").unit("{")
      ~ optionalWhitespace
      ~ syntax
      ~ optionalWhitespace
      ~ literal("}").unit("}"))

    def paren: Syntax[String, Char, Char, Value] = (openParen
      ~ optionalWhitespace
      ~ syntax
      ~ optionalWhitespace
      ~ closeParen)

    def array = (literal("[").unit("[")
      ~ optionalWhitespace
      ~ syntax
      ~ optionalWhitespace
      ~ literal("]").unit("]"))
  }
}
