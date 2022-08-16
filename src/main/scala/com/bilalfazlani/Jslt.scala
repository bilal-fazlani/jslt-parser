package com.bilalfazlani

import zio.Chunk

enum Jslt:
  case JPath(nodes: Chunk[JsltNode])
  case JIf(condition: JsltNode, jThen: Jslt, jElse: Jslt)
  case JMethodCall(method: String, args: Chunk[Jslt])
  case JArray(items: Chunk[Jslt])
  case jObject(items: Map[String, Jslt])
  case JString(value: String)
  case JNumber(value: Double)
  case JBoolean(value: Boolean)
