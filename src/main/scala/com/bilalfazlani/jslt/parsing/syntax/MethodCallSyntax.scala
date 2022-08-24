package com.bilalfazlani.jslt.parsing.syntax

import com.bilalfazlani.jslt.parsing.models.Jslt.JMethodCall
import zio.Chunk
import zio.parser.Syntax
import zio.parser.Syntax._
import zio.parser.SyntaxOps

trait MethodCallSyntax {
  this: JsltSyntax =>

  private[parsing] lazy val importName = charNotIn('"', ':', '(')
    .repeat
    .transform(_.mkString, (str: String) => Chunk.fromIterable(str)) ?? "importName"

  private[parsing] lazy val methodName = charNotIn('"', ':', '(')
    .repeat
    .transform(_.mkString, (str: String) => Chunk.fromIterable(str)) ?? "methodName"

  lazy val jMethodCallSyntax: Syntax[String, Char, Char, JMethodCall] = {
    (importName ~ optionalWhitespace
      ~ literal(":").unit(":") ~ optionalWhitespace
      ~ methodName ~ optionalWhitespace
      ~ jPathSyntax.separatedBy(comma).paren)
      .of[JMethodCall] ?? "method call"
  }
}
