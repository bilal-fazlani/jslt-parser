package com.bilalfazlani.jslt.parsing

import zio.Chunk

sealed trait Jslt
sealed trait JPrimitive extends Jslt

object Jslt {
  case class JPath(nodes: Chunk[JsltNode]) extends Jslt
  case class JIf(condition: JsltNode, jThen: Jslt, jElse: Jslt) extends Jslt
  case class JMethodCall(method: String, args: Chunk[Jslt]) extends Jslt
  case class JArray(items: Chunk[Jslt]) extends Jslt
  case class JObject(items: Map[String, Jslt]) extends Jslt

  object JPrimitive {
    case class JString(value: String) extends JPrimitive
    case class JDouble(value: Double) extends JPrimitive
    case class JInteger(value: Int) extends JPrimitive
    case class JBoolean(value: Boolean) extends JPrimitive
  }

  def parse(input: String) = JsltSyntax.jsltSyntax.parseString(input)
}