package com.bilalfazlani.jslt.parsing.models

import com.bilalfazlani.jslt.parsing.syntax.JsltFileSyntax
import zio.Chunk

case class JsltImport(path: String, name: String)

case class JsltFile(jsltImports: Chunk[JsltImport], content: Jslt.JObject)
object JsltFile {
  def parseFromString(contents: String) =
    JsltFileSyntax.fileSyntax.parseString(contents)
}
