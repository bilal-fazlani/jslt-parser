package com.bilalfazlani.jslt.parsing.syntax

import com.bilalfazlani.jslt.parsing.models.Jslt.JMethodCall
import zio.Chunk
import zio.parser.Syntax
import zio.parser.Syntax._

trait MethodCallSyntax {
  this: JsltSyntax =>

  private lazy val importName = charNotIn('"', ':', '(')
    .repeat
    .transform(_.mkString, (str: String) => Chunk.fromIterable(str))

  private lazy val methodName = charNotIn('"', ':', '(')
    .repeat
    .transform(_.mkString, (str: String) => Chunk.fromIterable(str))

  lazy val jMethodCallSyntax: Syntax[String, Char, Char, JMethodCall] = {
    (importName ~ optionalWhitespace
      ~ literal(":").unit(":") ~ optionalWhitespace
      ~ methodName ~ optionalWhitespace
      ~ jPathSyntax.separatedBy(comma).paren)
      .transform(
        { case (importName, methodName, jpath) => JMethodCall(importName, methodName, jpath) },
        (jMethodCall: JMethodCall) => (jMethodCall.importName, jMethodCall.method, jMethodCall.args)
      )
  }
}
