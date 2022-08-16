package com.bilalfazlani.jslt.parsing

opaque type JsltNode = String

object JsltNode {
  def apply(value: String): JsltNode = value
}

extension (node: JsltNode) {
  def toString: String = node
}
