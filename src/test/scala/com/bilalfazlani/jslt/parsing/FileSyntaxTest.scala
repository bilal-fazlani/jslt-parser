package com.bilalfazlani.jslt.parsing

import com.bilalfazlani.jslt.parsing.JPrimitive.{JDouble, JInteger}
import com.bilalfazlani.jslt.parsing.Jslt.*
import zio.Chunk
import zio.parser.Parser
import zio.test.Assertion.*
import zio.test.*
import zio.test.TestAspect.ignore

object FileSyntaxTest extends ZIOSpecDefault {
  def spec = suite("FileSyntaxTest")(
    test("parse single import statement") {
      val input: String = "import \"transformers/abcs.jslt\" as MyCustomProps"
      val expected: JsltImport = JsltImport(
        "transformers/abcs.jslt",
        "MyCustomProps"
      )
      val result = JsltSyntax.importSyntax.parseString(input)

      assert(result)(isRight(equalTo(expected)))
    },
    test("file syntax test without import") {
      val input = """{
          |"name": "john",
          |  "age": 30.2,
          |  "rank": 2,
          |  "is_admin": true,
          |  "path": .home.john
          |}"""
      val expected = JsltFile(
        Chunk.empty,
        JObject(
          Map(
            "name" -> JValue(JPrimitive.JString("john")),
            "age" -> JValue(JPrimitive.JDouble(30.2)),
            "rank" -> JValue(JPrimitive.JInteger(2)),
            "is_admin" -> JValue(JPrimitive.JBoolean(true)),
            "path" -> JPath(Chunk(JsltNode("home"), JsltNode("john")))
          )
        )
      )
      assert(JsltFile.parseFromString(input))(isRight(equalTo(expected)))
    } @@ ignore,
    test("file syntax test with 1 import") {
      val input = """import "transformers/abcs.jslt" as MyCustomProps
            |{
            |  "name": "john",
            |  "age": 30.2,
            |  "rank": 2,
            |  "is_admin": true,
            |  "path": .home.john
            |}""".stripMargin
      val expected = JsltFile(
        Chunk(JsltImport("transformers/abcs.jslt", "MyCustomProps")),
        JObject(
          Map(
            "name" -> JValue(JPrimitive.JString("john")),
            "age" -> JValue(JPrimitive.JDouble(30.2)),
            "rank" -> JValue(JPrimitive.JInteger(2)),
            "is_admin" -> JValue(JPrimitive.JBoolean(true)),
            "path" -> JPath(Chunk(JsltNode("home"), JsltNode("john")))
          )
        )
      )
      assert(JsltFile.parseFromString(input))(isRight(equalTo(expected)))
    },
    test("file syntax test with 2 imports") {
      val input = """import "transformers/abcs.jslt" as MyCustomProps
                |import "transformers/defs.jslt" as MyCustomProps2
                |{
                |  "name": "john",
                |  "age": 30.2,
                |  "rank": 2,
                |  "is_admin": true,
                |  "path": .home.john
                |}""".stripMargin
      val expected = JsltFile(
        Chunk(
          JsltImport("transformers/abcs.jslt", "MyCustomProps"),
          JsltImport("transformers/defs.jslt", "MyCustomProps2")
        ),
        JObject(
          Map(
            "name" -> JValue(JPrimitive.JString("john")),
            "age" -> JValue(JPrimitive.JDouble(30.2)),
            "rank" -> JValue(JPrimitive.JInteger(2)),
            "is_admin" -> JValue(JPrimitive.JBoolean(true)),
            "path" -> JPath(Chunk(JsltNode("home"), JsltNode("john")))
          )
        )
      )
      assert(JsltFile.parseFromString(input))(isRight(equalTo(expected)))
    },
    test("file syntax test with line gaps") {
      val input = """import "transformers/abcs.jslt" as MyCustomProps
                    |import "transformers/defs.jslt" as MyCustomProps2
                    |
                    |{
                    |  "name": "john",
                    |  "age": 30.2,
                    |  "rank": 2,
                    |  "is_admin": true,
                    |  "path": .home.john
                    |}""".stripMargin
      val expected = JsltFile(
        Chunk(
          JsltImport("transformers/abcs.jslt", "MyCustomProps"),
          JsltImport("transformers/defs.jslt", "MyCustomProps2")
        ),
        JObject(
          Map(
            "name" -> JValue(JPrimitive.JString("john")),
            "age" -> JValue(JPrimitive.JDouble(30.2)),
            "rank" -> JValue(JPrimitive.JInteger(2)),
            "is_admin" -> JValue(JPrimitive.JBoolean(true)),
            "path" -> JPath(Chunk(JsltNode("home"), JsltNode("john")))
          )
        )
      )
      assert(JsltFile.parseFromString(input))(isRight(equalTo(expected)))
    }
  )
}
