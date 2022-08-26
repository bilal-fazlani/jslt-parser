package com.bilalfazlani.jslt.parsing.syntax

import com.bilalfazlani.jslt.parsing.models.Jslt.JLiteral
import com.bilalfazlani.jslt.parsing.models.Jslt.JLiteral.{JBoolean, JDouble, JInteger, JString}
import com.bilalfazlani.jslt.parsing.models.JLiteral
import zio.Chunk
import zio.parser.Syntax
import zio.parser.Syntax.{digit, string}
import zio.parser.SyntaxOps
import zio.parser.StringErrSyntaxOps

trait LiteralSyntax extends JsltParsingConstructs {
  lazy val jStringSyntax: Syntax[String, Char, Char, JString] =
    (anyStringCustom | string("", "")).quoted
      .transform(
        x => JLiteral.JString(x),
        (jString: JString) => s"${jString.value}"
      ) ??"string"

  lazy val jBooleanSyntax: Syntax[String, Char, Char, JBoolean] = Syntax
    .oneOf(literal("true"), literal("false"))
    .transform(
      x => JLiteral.JBoolean(x.toBoolean),
      (jBool: JBoolean) => jBool.value.toString
    ) ?? "boolean"

  lazy val jDoubleSyntax: Syntax[String, Char, Char, JDouble] = {
    def toDouble(d: (Chunk[Char], Chunk[Char])): Double = d match {
      case (chunk, chunk2) =>
        (chunk.mkString + "." + chunk2.mkString).toDouble
    }

    def toString(
                  jNumber: JLiteral.JDouble
                ): (Chunk[Char], Chunk[Char]) = {
      jNumber.value.toString.split("\\.").toList match {
        case h :: t :: Nil =>
          (Chunk.fromIterable(h), Chunk.fromIterable(t))
        case _ => ??? //unreachable code
      }
    }

    (digit.repeat ~ (literal(".").unit(".") ~ digit.repeat))
      .transform(
        x => JLiteral.JDouble(toDouble(x)),
        (double: JDouble) => toString(double)
      ) ?? "double"
  }

  lazy val jIntegerSyntax: Syntax[String, Char, Char, JInteger] =
    digit.repeat
      .transform(
        x => JInteger(x.mkString.toInt),
        (int: JInteger) => Chunk.fromIterable(int.value.toString)
      ) ?? "integer"

  lazy val jLiteralSyntax: Syntax[String, Char, Char, JLiteral] =
    (jStringSyntax.widen[JLiteral] | jBooleanSyntax
      .widen[JLiteral] | jDoubleSyntax.widen[JLiteral] | jIntegerSyntax
      .widen[JLiteral]) ?? "primitive value"
}
