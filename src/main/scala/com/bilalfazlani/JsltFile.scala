package com.bilalfazlani

case class JsltImport(path: String, name: String)

case class JsltFile(jsltImports: Seq[JsltImport], content: Jslt.jObject)
