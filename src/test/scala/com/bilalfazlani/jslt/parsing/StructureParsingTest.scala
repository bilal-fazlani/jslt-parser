package com.bilalfazlani.jslt.parsing

import com.bilalfazlani.jslt.parsing.JPrimitive._
import com.bilalfazlani.jslt.parsing.Jslt._
import zio.Chunk
import zio.test._
import zio.test.Assertion._

object StructureParsingTest extends ZIOSpecDefault {
  def spec = suite("ObjectParsingTest")(
    test("parse flat object of primitive") {
      val input =
        """{ "name": "john", "age": 30.2, "rank": 2, "is_admin": true, "path": .home.john }"""
      val expected = JObject(
        Map(
          "name" -> JValue(JString("john")),
          "age" -> JValue(JDouble(30.2)),
          "rank" -> JValue(JInteger(2)),
          "is_admin" -> JValue(JBoolean(true)),
          "path" -> JPath(Chunk(JsltNode("home"), JsltNode("john")))
        )
      )
      val result = Jslt.parse(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse flat object with primitives with new lines") {
      val input =
        """{
            |  "name": "john",
            |  "age": 30.2,
            |  "rank": 2,
            |  "is_admin": true,
            |  "path": .home.john
            |}""".stripMargin
      val expected = JObject(
        Map(
          "name" -> JValue(JString("john")),
          "age" -> JValue(JDouble(30.2)),
          "rank" -> JValue(JInteger(2)),
          "is_admin" -> JValue(JBoolean(true)),
          "path" -> JPath(Chunk(JsltNode("home"), JsltNode("john")))
        )
      )
      val result = Jslt.parse(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse nested object of primitive") {
      val input =
        """{ "name": "john", "age": 30.2, "rank": 2, "is_admin": true, "address": { "street": "street1", "city": "city1" } }"""
      val expected = JObject(
        Map(
          "name" -> JValue(JString("john")),
          "age" -> JValue(JDouble(30.2)),
          "rank" -> JValue(JInteger(2)),
          "is_admin" -> JValue(JBoolean(true)),
          "address" -> JObject(
            Map(
              "street" -> JValue(JString("street1")),
              "city" -> JValue(JString("city1"))
            )
          )
        )
      )
      val result = Jslt.parse(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse nested object of nested objects of primitives on new lines") {
      val input =
        """{
                |  "name": "john",
                |  "age": 30.2,
                |  "rank": 2,
                |  "is_admin": true,
                |  "address": {
                |    "street": "street1",
                |    "city": "city1",
                |    "country": {  
                |      "name": "country1",  
                |      "code": "code1"  
                |    }  
                |  }
                |}""".stripMargin
      val expected = JObject(
        Map(
          "name" -> JValue(JString("john")),
          "age" -> JValue(JDouble(30.2)),
          "rank" -> JValue(JInteger(2)),
          "is_admin" -> JValue(JBoolean(true)),
          "address" -> JObject(
            Map(
              "street" -> JValue(JString("street1")),
              "city" -> JValue(JString("city1")),
              "country" -> JObject(
                Map(
                  "name" -> JValue(JString("country1")),
                  "code" -> JValue(JString("code1"))
                )
              )
            )
          )
        )
      )
      val result = Jslt.parse(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse object with array of primitive") {
      val input =
        """{ "name": "john", "age": 30.2, "rank": 2, "is_admin": true, "address": { "street": "street1", "city": "city1" }, "friends": [ "friend1", "friend2" ] }"""
      val expected = JObject(
        Map(
          "name" -> JValue(JString("john")),
          "age" -> JValue(JDouble(30.2)),
          "rank" -> JValue(JInteger(2)),
          "is_admin" -> JValue(JBoolean(true)),
          "address" -> JObject(
            Map(
              "street" -> JValue(JString("street1")),
              "city" -> JValue(JString("city1"))
            )
          ),
          "friends" -> JArray(
            Chunk(
              JValue(JString("friend1")),
              JValue(JString("friend2"))
            )
          )
        )
      )
      val result = Jslt.parse(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse object with array of object") {
      val input =
        """{ "name": "john", "age": 30.2, "rank": 2, "is_admin": true, "address": { "street": "street1", "city": "city1" }, "friends": [ { "name": "friend1", "age": 30.2, "path": .home.friend1 }, { "name": "friend2", "age": 30.2, "path": .home.friend2 } ] }"""
      val expected = JObject(
        Map(
          "name" -> JValue(JString("john")),
          "age" -> JValue(JDouble(30.2)),
          "rank" -> JValue(JInteger(2)),
          "is_admin" -> JValue(JBoolean(true)),
          "address" -> JObject(
            Map(
              "street" -> JValue(JString("street1")),
              "city" -> JValue(JString("city1"))
            )
          ),
          "friends" -> JArray(
            Chunk(
              JObject(
                Map(
                  "name" -> JValue(JString("friend1")),
                  "age" -> JValue(JDouble(30.2)),
                  "path" -> JPath(Chunk(JsltNode("home"), JsltNode("friend1")))
                )
              ),
              JObject(
                Map(
                  "name" -> JValue(JString("friend2")),
                  "age" -> JValue(JDouble(30.2)),
                  "path" -> JPath(Chunk(JsltNode("home"), JsltNode("friend2")))
                )
              )
            )
          )
        )
      )
      val result = Jslt.parse(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse object with array of object with array of primitive") {
      val input =
        """{ "name": "john", "age": 30.2, "rank": 2, "is_admin": true, "address": { "street": "street1", "city": "city1" }, "friends": [ { "name": "friend1", "age": 30.2, "friends": [ "friend1", "friend2" ] }, { "name": "friend2", "age": 30.2, "friends": [ "friend1", "friend2" ] } ] }"""
      val expected = JObject(
        Map(
          "name" -> JValue(JString("john")),
          "age" -> JValue(JDouble(30.2)),
          "rank" -> JValue(JInteger(2)),
          "is_admin" -> JValue(JBoolean(true)),
          "address" -> JObject(
            Map(
              "street" -> JValue(JString("street1")),
              "city" -> JValue(JString("city1"))
            )
          ),
          "friends" -> JArray(
            Chunk(
              JObject(
                Map(
                  "name" -> JValue(JString("friend1")),
                  "age" -> JValue(JDouble(30.2)),
                  "friends" -> JArray(
                    Chunk(
                      JValue(JString("friend1")),
                      JValue(JString("friend2"))
                    )
                  )
                )
              ),
              JObject(
                Map(
                  "name" -> JValue(JString("friend2")),
                  "age" -> JValue(JDouble(30.2)),
                  "friends" -> JArray(
                    Chunk(
                      JValue(JString("friend1")),
                      JValue(JString("friend2"))
                    )
                  )
                )
              )
            )
          )
        )
      )
      val result = Jslt.parse(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse array of paths with new lines") {
      val input =
        """[
          |  .home.john,
          |  .home.john.age,
          |  .home.john.rank
          |]
          |""".stripMargin
      val expected = JArray(
        Chunk(
          JPath(Chunk(JsltNode("home"), JsltNode("john"))),
          JPath(Chunk(JsltNode("home"), JsltNode("john"), JsltNode("age"))),
          JPath(Chunk(JsltNode("home"), JsltNode("john"), JsltNode("rank")))
        )
      )
      val result = Jslt.parse(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse array of strings with new lines") {
      val input =
        """[
            |  "john",
            |  "john's age",
            |  "john's rank"
            |]
            |""".stripMargin
      val expected = JArray(
        Chunk(
          JValue(JString("john")),
          JValue(JString("john's age")),
          JValue(JString("john's rank"))
        )
      )
      val result = Jslt.parse(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse array of integers on new lines") {
      val input =
        """[
            |  1,
            |  2,
            |  3
            |]
            |""".stripMargin
      val expected = JArray(
        Chunk(
          JValue(JInteger(1)),
          JValue(JInteger(2)),
          JValue(JInteger(3))
        )
      )
      val result = Jslt.parse(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse array of objects on new lines") {
      val input =
        """[
                |  { "name": "john", "age": 30.2, "rank": 2, "is_admin": true, "address": { "street": "street1", "city": "city1" } },
                |  { "name": "john", "age": 30.2, "rank": 2, "is_admin": true, "address": { "street": "street1", "city": "city1" } }
                |]
                |""".stripMargin
      val expected = JArray(
        Chunk(
          JObject(
            Map(
              "name" -> JValue(JString("john")),
              "age" -> JValue(JDouble(30.2)),
              "rank" -> JValue(JInteger(2)),
              "is_admin" -> JValue(JBoolean(true)),
              "address" -> JObject(
                Map(
                  "street" -> JValue(JString("street1")),
                  "city" -> JValue(JString("city1"))
                )
              )
            )
          ),
          JObject(
            Map(
              "name" -> JValue(JString("john")),
              "age" -> JValue(JDouble(30.2)),
              "rank" -> JValue(JInteger(2)),
              "is_admin" -> JValue(JBoolean(true)),
              "address" -> JObject(
                Map(
                  "street" -> JValue(JString("street1")),
                  "city" -> JValue(JString("city1"))
                )
              )
            )
          )
        )
      )
      val result = Jslt.parse(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse flat object of primitives with extra comma in the end") {
      val input =
        """{ "name": "john", "age": 30.2, "rank": 2, "is_admin": true, "address": { "street": "street1", "city": "city1", }}"""
      val expected = JObject(
        Map(
          "name" -> JValue(JString("john")),
          "age" -> JValue(JDouble(30.2)),
          "rank" -> JValue(JInteger(2)),
          "is_admin" -> JValue(JBoolean(true)),
          "address" -> JObject(
            Map(
              "street" -> JValue(JString("street1")),
              "city" -> JValue(JString("city1"))
            )
          )
        )
      )
      val result = Jslt.parse(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse array of primitives with extra comma in the end") {
      val input =
        """[ "john", "john's age", "john's rank", ]"""
      val expected = JArray(
        Chunk(
          JValue(JString("john")),
          JValue(JString("john's age")),
          JValue(JString("john's rank"))
        )
      )
      val result = Jslt.parse(input)
      assert(result)(isRight(equalTo(expected)))
    }
  )
}
