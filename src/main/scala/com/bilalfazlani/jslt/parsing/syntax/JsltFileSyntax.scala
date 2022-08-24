package com.bilalfazlani.jslt.parsing.syntax

import com.bilalfazlani.jslt.parsing.models.{JsltFile, JsltImport}
import zio.Chunk
import zio.parser.Syntax.anyChar
import zio.parser.{Syntax, SyntaxOps}

object JsltFileSyntax extends JsltSyntax {
  lazy val importSyntax: Syntax[String, Char, Char, JsltImport] = (literal("import").unit("import")
    ~ optionalWhitespace
    ~ anyStringCustom.quoted
    ~ optionalWhitespace
    ~ literal("as").unit("as")
    ~ optionalWhitespace
    ~ anyChar
    .repeatUntil(newLine)
    .transform(
      x => x.mkString,
      (str: String) => Chunk.fromIterable(str)
    )).of[JsltImport]

  lazy val fileSyntax: Syntax[String, Char, Char, JsltFile] =
    (importSyntax.repeat0 ~ optionalWhitespace ~ jObjectSyntax)
      .of[JsltFile] ?? "jslt file"
}
