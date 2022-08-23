package com.bilalfazlani.jslt.parsing.syntax

import com.bilalfazlani.jslt.parsing.models.Jslt.JValue
import com.bilalfazlani.jslt.parsing.models.Jslt.JValue.{JBoolean, JDouble, JInteger, JString}
import com.bilalfazlani.jslt.parsing.models.JPrimitive
import zio.Chunk
import zio.parser.Syntax
import zio.parser.Syntax.digit

trait PrimitiveSyntax extends JsltParsingConstructs {
  lazy val jStringSyntax: Syntax[String, Char, Char, JString] =
    anyStringCustom.quoted
      .transform(
        x => JValue.JString(x),
        (jString: JString) => s"${jString.value}"
      )

  lazy val jBooleanSyntax: Syntax[String, Char, Char, JBoolean] = Syntax
    .oneOf(literal("true"), literal("false"))
    .transform(
      x => JValue.JBoolean(x.toBoolean),
      (jBool: JBoolean) => jBool.value.toString
    )

  lazy val jDoubleSyntax: Syntax[String, Char, Char, JDouble] = {
    def toDouble(d: (Chunk[Char], Chunk[Char])): Double = d match {
      case (chunk, chunk2) =>
        (chunk.mkString + "." + chunk2.mkString).toDouble
    }

    def toString(
                  jNumber: JValue.JDouble
                ): (Chunk[Char], Chunk[Char]) = {
      jNumber.value.toString.split("\\.").toList match {
        case h :: t :: Nil =>
          (Chunk.fromIterable(h), Chunk.fromIterable(t))
      }
    }

    (digit.repeat ~ (literal(".").unit(".") ~ digit.repeat))
      .transform(
        x => JValue.JDouble(toDouble(x)),
        (double: JDouble) => toString(double)
      )
  }

  lazy val jIntegerSyntax: Syntax[String, Char, Char, JInteger] =
    digit.repeat
      .transform(
        x => JInteger(x.mkString.toInt),
        (int: JInteger) => Chunk.fromIterable(int.value.toString)
      )

  lazy val jPrimitiveSyntax: Syntax[String, Char, Char, JPrimitive] =
    jStringSyntax.widen[JPrimitive] | jBooleanSyntax
      .widen[JPrimitive] | jDoubleSyntax.widen[JPrimitive] | jIntegerSyntax
      .widen[JPrimitive]
}