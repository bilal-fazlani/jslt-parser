package com.bilalfazlani.jslt.parsing

import com.bilalfazlani.jslt.parsing.Jslt.JPrimitive
import com.bilalfazlani.jslt.parsing.Jslt.JPrimitive.{JBoolean, JDouble, JInteger, JString}
import zio.Chunk
import zio.parser.Syntax
import zio.parser.Syntax.digit

trait PrimitiveSyntax extends JsltParsingConstructs {
  def jStringSyntax: Syntax[String, Char, Char, JString] =
    anyStringCustom.quoted
      .transform(
        x => JPrimitive.JString(x),
        (jString: JString) => s"${jString.value}"
      )

  def jBooleanSyntax: Syntax[String, Char, Char, JBoolean] = Syntax
    .oneOf(literal("true"), literal("false"))
    .transform(
      x => JPrimitive.JBoolean(x.toBoolean),
      (jBool: JBoolean) => jBool.value.toString
    )

  def jDoubleSyntax: Syntax[String, Char, Char, JDouble] = {
    def toDouble(d: (Chunk[Char], Chunk[Char])): Double = d match {
      case (chunk, chunk2) =>
        (chunk.mkString + "." + chunk2.mkString).toDouble
    }

    def toString(
                  jNumber: JPrimitive.JDouble
                ): (Chunk[Char], Chunk[Char]) = {
      jNumber.value.toString.split("\\.").toList match {
        case h :: t :: Nil =>
          (Chunk.fromIterable(h), Chunk.fromIterable(t))
      }
    }

    (digit.repeat ~ (literal(".").unit(".") ~ digit.repeat))
      .transform(
        x => JPrimitive.JDouble(toDouble(x)),
        (double: JDouble) => toString(double)
      )
  }

  def jIntegerSyntax: Syntax[String, Char, Char, JInteger] =
    digit.repeat
      .transform(
        x => JInteger(x.mkString.toInt),
        (int: JInteger) => Chunk.fromIterable(int.value.toString)
      )

  def jPrimitiveSyntax: Syntax[String, Char, Char, JPrimitive] =
    jStringSyntax.widen[JPrimitive] | jBooleanSyntax
      .widen[JPrimitive] | jDoubleSyntax.widen[JPrimitive] | jIntegerSyntax
      .widen[JPrimitive]
}
