package com.bilalfazlani.jslt.parsing

import com.bilalfazlani.jslt.parsing.JPrimitive.*
import com.bilalfazlani.jslt.parsing.Jslt.*
import zio.Chunk
import zio.test.*
import zio.test.Assertion.*

object StructureParsingTest extends ZIOSpecDefault {
  def spec = suite("ObjectParsingTest")(
    test("parse flat object of primitive") {
      val input =
        """{ "name": "john", "age": 30.2, "rank": 2, "is_admin": true }"""
      val expected = JObject(
        Map(
          "name" -> JValue(JString("john")),
          "age" -> JValue(JDouble(30.2)),
          "rank" -> JValue(JInteger(2)),
          "is_admin" -> JValue(JBoolean(true))
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
        """{ "name": "john", "age": 30.2, "rank": 2, "is_admin": true, "address": { "street": "street1", "city": "city1" }, "friends": [ { "name": "friend1", "age": 30.2 }, { "name": "friend2", "age": 30.2 } ] }"""
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
                  "age" -> JValue(JDouble(30.2))
                )
              ),
              JObject(
                Map(
                  "name" -> JValue(JString("friend2")),
                  "age" -> JValue(JDouble(30.2))
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
    }
  )
}
