package com.bilalfazlani.jslt.parsing

case class JsltImport(path: String, name: String)

case class JsltFile(jsltImports: Seq[JsltImport], content: Jslt.JObject)
