package com.bilalfazlani

import zio.Chunk

sealed trait Jslt
object Jslt {
  case class JPath(nodes: Chunk[JsltNode]) extends Jslt
  case class JIf(condition: JsltNode, jThen: Jslt, jElse: Jslt) extends Jslt
  case class JMethodCall(method: String, args: Chunk[Jslt]) extends Jslt
  case class JArray(items: Chunk[Jslt]) extends Jslt
  case class JObject(items: Map[String, Jslt]) extends Jslt
  case class JString(value: String) extends Jslt
  case class JNumber(value: Double) extends Jslt
  case class JBoolean(value: Boolean) extends Jslt
}
