package com.bilalfazlani.jslt.parsing

import zio.parser.*
import zio.parser.Syntax.*
import zio.Chunk
import Syntax.*
import com.bilalfazlani.jslt.parsing.JsltSyntax.*

val input =
  """{"name": ["bilal", { "dd": 3.3 }],
    |"rank": [24.5, 4],
    |"age": 2}
    |""".stripMargin.trim

@main def main(): Unit = {
  println(jObjectSyntax.parseString(input))
}
