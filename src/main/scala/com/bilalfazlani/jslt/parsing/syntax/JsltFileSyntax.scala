package com.bilalfazlani.jslt.parsing.syntax

import com.bilalfazlani.jslt.parsing.models
import com.bilalfazlani.jslt.parsing.models.{JsltFile, JsltImport}
import zio.Chunk
import zio.parser.Syntax.anyChar
import zio.parser.SyntaxOps

object JsltFileSyntax extends JsltSyntax {
  lazy val importSyntax = (literal("import").unit("import")
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
    )).transform(
    { case (path, name) =>
      JsltImport(path, name)
    },
    (jImport: JsltImport) => (jImport.path, jImport.name)
  )

  lazy val fileSyntax =
    (importSyntax.repeat0 ~ optionalWhitespace ~ jObjectSyntax)
      .transform(
        { case (imports, obj) =>
          models.JsltFile(imports, obj)
        },
        (jslt: JsltFile) => (jslt.jsltImports, jslt.content)
      )
}
