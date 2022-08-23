package com.bilalfazlani.jslt.parsing.syntax

import com.bilalfazlani.jslt.parsing.models.Jslt.JPath
import com.bilalfazlani.jslt.parsing.models.JsltNode
import zio.Chunk
import zio.parser.Syntax
import zio.parser.Syntax.alphaNumeric

trait JPathSyntax extends JsltParsingConstructs {
  lazy val jPathSyntax: Syntax[String, Char, Char, JPath] =
    literal(".").unit(".") ~ (alphaNumeric <> acceptableChars).repeat
      .transform(
        _.mkString,
        (str: String) => Chunk.fromIterable(str)
      )
      .repeatWithSep(literal(".").unit("."))
      .transform(
        strs => JPath(Chunk.fromIterable(strs.map(JsltNode.apply))),
        (path: JPath) => path.nodes.map(_.toString)
      )
}
