package com.bilalfazlani.jslt.parsing.syntax

import com.bilalfazlani.jslt.parsing.models.Jslt.JPath
import com.bilalfazlani.jslt.parsing.models.JsltNode
import zio.Chunk
import zio.parser.Syntax.alphaNumeric
import zio.parser.{Syntax, SyntaxOps}

trait JPathSyntax extends JsltParsingConstructs {

  lazy val jPathSyntax: Syntax[String, Char, Char, JPath] =
    literal(".").unit(".") ~ (alphaNumeric | acceptableSymbols).repeat
      .mkString
      .repeatWithSep(literal(".").unit("."))
      .transform(
        strs => JPath(Chunk.fromIterable(strs.map(JsltNode.apply))),
        (path: JPath) => path.nodes.map(_.toString)
      ) ?? "jslt path"
}
