package com.bilalfazlani.jslt.parsing

import zio.Chunk
import zio.parser.Parser

case class JsltImport(path: String, name: String)

case class JsltFile(jsltImports: Chunk[JsltImport], content: Jslt.JObject)
object JsltFile {
  def parseFromString(contents: String) =
    JsltSyntax.fileSyntax.parseString(contents)
}
