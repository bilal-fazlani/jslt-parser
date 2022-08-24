package com.bilalfazlani.jslt.parsing.syntax

import zio.Chunk
import zio.parser.Syntax
import zio.parser.Syntax.{char, charIn, string, whitespace}
import zio.parser.SyntaxOps

trait JsltParsingConstructs {
  def literal(lit: => String): Syntax[String, Char, Char, String] =
    string(lit, lit)

  lazy val optionalWhitespace: Syntax[String, Char, Char, Unit] =
    whitespace.repeat0.unit(Chunk.empty)

  lazy val requiredWhitespace: Syntax[String, Char, Char, Unit] =
    whitespace.repeat.unit(Chunk.empty)

  lazy val newLine = char('\n')

  lazy val openParen = optionalWhitespace ~ literal("(").unit("(")
  lazy val closeParen = optionalWhitespace ~ literal(")").unit(")")

  lazy val anyStringCustom: Syntax[String, Char, Char, String] =
    Syntax.notChar('"')
      .repeat
      .transform(_.mkString, str => Chunk.fromIterable(str))

  lazy val comma: Syntax[String, Char, Char, Unit] = literal(",").unit(", ")

  lazy val colon: Syntax[String, Char, Char, Unit] = literal(":").unit(": ")

  implicit class SyntaxExtensions[Value](
                                          syntax: => Syntax[String, Char, Char, Value]
                                        ) {
    private[parsing] lazy val memoized = syntax

    lazy val quoted: Syntax[String, Char, Char, Value] = memoized.between(
      literal("\"").unit("\""),
      literal("\"").unit("\"")
    )

    def separatedBy(separator: => Syntax[String, Char, Char, Unit]): Syntax[String, Char, Char, Chunk[Value]] =
      memoized.repeatWithSep(
        optionalWhitespace ~ separator ~ optionalWhitespace
      )

    lazy val withTrailingComma: Syntax[String, Char, Char, Value] = memoized ~ comma.optional.unit(None)

    lazy val curly: Syntax[String, Char, Char, Value] = (literal("{").unit("{")
      ~ optionalWhitespace
      ~ memoized
      ~ optionalWhitespace
      ~ literal("}").unit("}"))

    lazy val paren: Syntax[String, Char, Char, Value] = (openParen
      ~ optionalWhitespace
      ~ memoized
      ~ optionalWhitespace
      ~ closeParen)

    lazy val optionalParen: Syntax[String, Char, Char, Value] = memoized.paren | memoized

    lazy val array: Syntax[String, Char, Char, Value] = (literal("[").unit("[")
      ~ optionalWhitespace
      ~ memoized
      ~ optionalWhitespace
      ~ literal("]").unit("]"))
  }
}
