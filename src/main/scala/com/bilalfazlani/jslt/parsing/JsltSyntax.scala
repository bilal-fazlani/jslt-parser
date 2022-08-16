package com.bilalfazlani.jslt.parsing

import zio.Chunk
import zio.parser.*
import zio.parser.Syntax
import zio.parser.Syntax.*
import Jslt.*

object JsltSyntax {
  def literal(lit: String): Syntax[String, Char, Char, String] =
    string(lit, lit)

  val optionalWhitespace: Syntax[String, Char, Char, Unit] =
    whitespace.repeat0.unit(Chunk.empty)

  val alphanumericString: Syntax[String, Char, Char, String] =
    alphaNumeric.repeat
      .transform(_.mkString, Chunk.fromIterable)

  val anyStringCustom: Syntax[String, Char, Char, String] =
    Syntax.notChar('"').repeat.transform(_.mkString, Chunk.fromIterable)

  val comma: Syntax[String, Char, Char, Unit] = literal(",").unit(", ")

  val colon: Syntax[String, Char, Char, Unit] = literal(":").unit(": ")

  extension [Err, In, Out, Value](syntax: Syntax[Err, In, Out, Value]) {
    def quoted = syntax.between(
      literal("\"").unit("\""),
      literal("\"").unit("\"")
    )

    def curly = literal("{").unit("{")
      ~ optionalWhitespace
      ~ syntax
      ~ optionalWhitespace
      ~ literal("}").unit("}")

    def array = literal("[").unit("[")
      ~ optionalWhitespace
      ~ syntax
      ~ optionalWhitespace
      ~ literal("]").unit("]")
  }

  def jStringSyntax: Syntax[Any, Char, Any, Jslt] =
    anyStringCustom.quoted
      .transform(
        x => JValue(JPrimitive.JString(x)),
        (jslt: Jslt) => jslt.asInstanceOf[JPrimitive.JString].value
      )

  def jBooleanSyntax: Syntax[String, Char, Char, Jslt] = Syntax
    .oneOf(literal("true"), literal("false"))
    .transform(
      x => JValue(JPrimitive.JBoolean(x.toBoolean)),
      (jslt: Jslt) => jslt.asInstanceOf[JPrimitive.JBoolean].value.toString
    )

  def jDoubleSyntax: Syntax[String, Char, Char, Jslt] =
    def toDouble(d: (Chunk[Char], Option[Chunk[Char]])): Double = d match {
      case (chunk, None) => chunk.mkString.toDouble
      case (chunk, Some(chunk2)) =>
        (chunk.mkString + "." + chunk2.mkString).toDouble
    }

    def toString(
        jNumber: JPrimitive.JDouble
    ): (Chunk[Char], Option[Chunk[Char]]) =
      jNumber.value.toString.split(".").toList match {
        case h :: Nil => (Chunk.fromIterable(h), None)
        case h :: t :: Nil =>
          (Chunk.fromIterable(h), Some(Chunk.fromIterable(t)))
      }

    (digit.repeat ~ (literal(".").unit(".") ~ digit.repeat).optional)
      .transform(
        x => JValue(JPrimitive.JDouble(toDouble(x))),
        (jslt: Jslt) =>
          toString(
            jslt.asInstanceOf[JValue].value.asInstanceOf[JPrimitive.JDouble]
          )
      )

  def jPrimitiveSyntax: Syntax[Any, Char, Any, Jslt] =
    jBooleanSyntax <> jDoubleSyntax <> jStringSyntax

  def jArraySyntax: Syntax[Any, Char, Any, Jslt] =
    (jPrimitiveSyntax <> jObjectSyntax)
      .repeatWithSep(
        optionalWhitespace ~ comma ~ optionalWhitespace
      )
      .array
      .transform(
        items => JArray(items),
        (jslt: Jslt) => jslt.asInstanceOf[JArray].items
      )

  def jsltSyntax: Syntax[Any, Char, Any, Jslt] =
    jArraySyntax <> jPrimitiveSyntax <> jObjectSyntax

  val keySyntax: Syntax[Any, Char, Any, String] =
    alphanumericString.quoted
      .named("json key")

  val keyValueSyntax: Syntax[Any, Char, Any, (String, Jslt)] = keySyntax
    ~ optionalWhitespace
    ~ colon
    ~ optionalWhitespace
    ~ jsltSyntax

  def jObjectSyntax: Syntax[Any, Char, Any, Jslt] =
    keyValueSyntax
      .repeatWithSep(optionalWhitespace ~ comma ~ optionalWhitespace)
      .curly
      .transform(
        items => JObject(items.toMap),
        (obj: Jslt) => Chunk.fromIterable(obj.asInstanceOf[JObject].items)
      )
}
