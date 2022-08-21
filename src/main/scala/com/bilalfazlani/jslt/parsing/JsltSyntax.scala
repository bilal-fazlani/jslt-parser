package com.bilalfazlani.jslt.parsing

import zio.Chunk
import zio.parser._
import zio.parser.Syntax
import zio.parser.Syntax._
import Jslt._
import com.bilalfazlani.jslt.parsing.Jslt.JPrimitive._

object JsltSyntax {
  def literal(lit: String): Syntax[String, Char, Char, String] =
    string(lit, lit)

  val optionalWhitespace: Syntax[String, Char, Char, Unit] =
    whitespace.repeat0.unit(Chunk.empty)

  val acceptableChars: Syntax[String, Char, Char, Char] = charIn("_-")

  val newLine = char('\n')

  val anyStringCustom: Syntax[String, Char, Char, String] =
    Syntax
      .notChar('"')
      .repeat
      .transform(_.mkString, str => Chunk.fromIterable(str))

  val comma: Syntax[String, Char, Char, Unit] = literal(",").unit(", ")

  val colon: Syntax[String, Char, Char, Unit] = literal(":").unit(": ")

  implicit class SyntaxExtensions[Value](
                                          syntax: Syntax[String, Char, Char, Value]
                                        ) {
    def quoted: Syntax[String, Char, Char, Value] = syntax.between(
      literal("\"").unit("\""),
      literal("\"").unit("\"")
    )

    def separatedBy(separator: Syntax[String, Char, Char, Unit]) =
      syntax.repeatWithSep(
        optionalWhitespace ~ separator ~ optionalWhitespace
      )

    def withTrailingComma = syntax ~ comma.optional.unit(None)

    def curly = (literal("{").unit("{")
      ~ optionalWhitespace
      ~ syntax
      ~ optionalWhitespace
      ~ literal("}").unit("}"))

    def array = (literal("[").unit("[")
      ~ optionalWhitespace
      ~ syntax
      ~ optionalWhitespace
      ~ literal("]").unit("]"))
  }

  def jStringSyntax: Syntax[String, Char, Char, JString] =
    anyStringCustom.quoted
      .transform(
        x => JPrimitive.JString(x),
        (jString: JString) => s"${jString.value}"
      )

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

  def jArraySyntax: Syntax[String, Char, Char, JArray] =
    (
      jPrimitiveSyntax.widen[Jslt] |
        jObjectSyntax.widen[Jslt] |
        jPathSyntax.widen[Jslt]
      )
      .separatedBy(comma)
      .withTrailingComma
      .array
      .transform(
        items => JArray(items),
        (arr: JArray) => arr.items
      )

  def jObjectSyntax: Syntax[String, Char, Char, JObject] =
    keyValueSyntax
      .separatedBy(comma)
      .withTrailingComma
      .curly
      .transform(
        items => JObject(items.toMap),
        (obj: JObject) => Chunk.fromIterable(obj.items)
      )

  val importSyntax = (literal("import").unit("import")
    ~ optionalWhitespace
    ~ anyStringCustom.quoted
    ~ optionalWhitespace
    ~ literal("as").unit("as")
    ~ optionalWhitespace
    ~ anyChar
    .repeatUntil(newLine)
    .transform(
      x => x.mkString,
      (str: String) => Chunk.fromIterable(str)
    )).transform(
    { case (path, name) =>
      JsltImport(path, name)
    },
    (jImport: JsltImport) => (jImport.path, jImport.name)
  )

  val fileSyntax =
    (importSyntax.repeat0 ~ optionalWhitespace ~ jObjectSyntax)
      .transform(
        { case (imports, obj) =>
          JsltFile(imports, obj)
        },
        (jslt: JsltFile) => (jslt.jsltImports, jslt.content)
      )

  def jsltSyntax: Syntax[String, Char, Char, Jslt] =
    jArraySyntax.widen[Jslt] |
      jObjectSyntax.widen[Jslt] |
      jPrimitiveSyntax.widen[Jslt] |
      jPathSyntax.widen[Jslt]

  val keySyntax: Syntax[String, Char, Char, String] =
    anyStringCustom.quoted
      .named("json key")

  val keyValueSyntax: Syntax[String, Char, Char, (String, Jslt)] = (keySyntax
    ~ optionalWhitespace
    ~ colon
    ~ optionalWhitespace
    ~ jsltSyntax)
}
