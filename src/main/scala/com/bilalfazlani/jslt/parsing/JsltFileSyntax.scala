package com.bilalfazlani.jslt.parsing

import zio.Chunk
import zio.parser.Syntax.anyChar

object JsltFileSyntax extends JsltSyntax {
  val importSyntax = (literal("import").unit("import")
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

  val fileSyntax =
    (importSyntax.repeat0 ~ optionalWhitespace ~ jObjectSyntax)
      .transform(
        { case (imports, obj) =>
          JsltFile(imports, obj)
        },
        (jslt: JsltFile) => (jslt.jsltImports, jslt.content)
      )
}