package com.bilalfazlani.jslt.parsing

import com.bilalfazlani.jslt.parsing.Jslt.JPath
import zio.Chunk
import zio.parser.Syntax
import zio.parser.Syntax.alphaNumeric

trait JPathSyntax extends JsltParsingConstructs {
  def jPathSyntax: Syntax[String, Char, Char, JPath] =
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
