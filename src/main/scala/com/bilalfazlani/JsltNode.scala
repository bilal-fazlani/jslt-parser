package com.bilalfazlani

opaque type JsltNode = String

object JsltNode {
  def apply(value: String): JsltNode = value
}

extension (node: JsltNode) {
  def toString: String = node
}
