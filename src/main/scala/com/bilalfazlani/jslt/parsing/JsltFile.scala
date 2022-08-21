package com.bilalfazlani.jslt.parsing

import zio.Chunk

case class JsltImport(path: String, name: String)

case class JsltFile(jsltImports: Chunk[JsltImport], content: Jslt.JObject)
object JsltFile {
  def parseFromString(contents: String) =
    JsltFileSyntax.fileSyntax.parseString(contents)
}
