package com.bilalfazlani.jslt.parsing

import com.bilalfazlani.jslt.parsing.models.Jslt._
import com.bilalfazlani.jslt.parsing.models.{JsltFile, JsltImport, JsltNode}
import com.bilalfazlani.jslt.parsing.syntax.JsltFileSyntax
import zio.Chunk
import zio.test.Assertion._
import zio.test._

object FileSyntaxTest extends ZIOSpecDefault {
  def spec = suite("FileSyntaxTest")(
    test("parse single import statement") {
      val input: String = "import \"transformers/abcs.jslt\" as MyCustomProps"
      val expected: JsltImport = JsltImport(
        "transformers/abcs.jslt",
        "MyCustomProps"
      )
      val result = JsltFileSyntax.importSyntax.parseString(input)

      assert(result)(isRight(equalTo(expected)))
    },
    test("file syntax test without import") {
      val input = """{
          |  "name": "john",
          |  "age": 30.2,
          |  "rank": 2,
          |  "is_admin": true,
          |  "path": .home.john
          |}""".stripMargin
      val expected = JsltFile(
        Chunk.empty,
        JObject(
          Map(
            "name" -> JLiteral.JString("john"),
            "age" -> JLiteral.JDouble(30.2),
            "rank" -> JLiteral.JInteger(2),
            "is_admin" -> JLiteral.JBoolean(true),
            "path" -> JPath(Chunk(JsltNode("home"), JsltNode("john")))
          )
        )
      )
      assert(JsltFile.parseFromString(input))(isRight(equalTo(expected)))
    },
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
            "name" -> JLiteral.JString("john"),
            "age" -> JLiteral.JDouble(30.2),
            "rank" -> JLiteral.JInteger(2),
            "is_admin" -> JLiteral.JBoolean(true),
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
            "name" -> JLiteral.JString("john"),
            "age" -> JLiteral.JDouble(30.2),
            "rank" -> JLiteral.JInteger(2),
            "is_admin" -> JLiteral.JBoolean(true),
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
            "name" -> JLiteral.JString("john"),
            "age" -> JLiteral.JDouble(30.2),
            "rank" -> JLiteral.JInteger(2),
            "is_admin" -> JLiteral.JBoolean(true),
            "path" -> JPath(Chunk(JsltNode("home"), JsltNode("john")))
          )
        )
      )
      assert(JsltFile.parseFromString(input))(isRight(equalTo(expected)))
    }
  )
}
