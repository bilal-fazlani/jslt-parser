package com.bilalfazlani

import zio.parser.*
import zio.parser.Syntax.*
import zio.Chunk
import Syntax.*
import com.bilalfazlani.JsltSyntax.*

val input =
  """{"name": "bilal",
    |"rank": 24.5,
    |"age": 2}
    |""".stripMargin.trim

@main def main(): Unit = {
  println(jObjectSyntax.parseString(input))
}
