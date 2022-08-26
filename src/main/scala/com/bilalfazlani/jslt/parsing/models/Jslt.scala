package com.bilalfazlani.jslt.parsing.models

import com.bilalfazlani.jslt.parsing.syntax.JsltSyntax
import zio.Chunk

sealed trait Jslt

sealed trait JLiteral extends Jslt

object Jslt {
  case class JPath(nodes: Chunk[JsltNode]) extends Jslt

  object JPath {
    def apply(nodes: JsltNode*): JPath = JPath(Chunk.fromIterable(nodes))
  }

  case class JIf(condition: BooleanExpression, ifTrue: Jslt, ifFalse: Option[Jslt]) extends Jslt

  case class JMethodCall(importName: String, method: String, args: Chunk[Jslt.JPath]) extends Jslt

  case class JArray(items: Chunk[Jslt]) extends Jslt

  case class JObject(items: Map[String, Jslt]) extends Jslt

  object JLiteral {
    case class JString(value: String) extends JLiteral

    case class JDouble(value: Double) extends JLiteral

    case class JInteger(value: Int) extends JLiteral

    case class JBoolean(value: Boolean) extends JLiteral
  }

  def parse(input: String) =
    new JsltSyntax {}.jsltSyntax.parseString(input)
}