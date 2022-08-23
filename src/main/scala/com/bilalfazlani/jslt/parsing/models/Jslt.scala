package com.bilalfazlani.jslt.parsing.models

import com.bilalfazlani.jslt.parsing.syntax.JsltSyntax
import zio.Chunk

sealed trait Jslt

sealed trait JPrimitive extends Jslt

object Jslt {
  case class JPath(nodes: Chunk[JsltNode]) extends Jslt

  object JPath {
    def apply(nodes: JsltNode*): JPath = JPath(Chunk.fromIterable(nodes))
  }

  case class JIf(condition: BooleanExpression, ifTrue: Jslt, ifFalse: Option[Jslt]) extends Jslt

  case class JMethodCall(importName: String, method: String, args: Chunk[Jslt.JPath]) extends Jslt

  case class JArray(items: Chunk[Jslt]) extends Jslt

  case class JObject(items: Map[String, Jslt]) extends Jslt

  object JValue {
    case class JString(value: String) extends JPrimitive

    case class JDouble(value: Double) extends JPrimitive

    case class JInteger(value: Int) extends JPrimitive

    case class JBoolean(value: Boolean) extends JPrimitive
  }

  def parse(input: String) =
    new JsltSyntax {}.jsltSyntax.parseString(input)
}