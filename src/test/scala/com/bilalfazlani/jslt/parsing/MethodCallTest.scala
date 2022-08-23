package com.bilalfazlani.jslt.parsing

import com.bilalfazlani.jslt.parsing.models.Jslt._
import com.bilalfazlani.jslt.parsing.models.{Jslt, JsltNode}
import zio.Chunk
import zio.test.Assertion._
import zio.test._

object MethodCallTest extends ZIOSpecDefault {
  def spec = suite("MethodCallTest")(
    test("parse method with single arg") {
      val input =
        "MyClass:myMethod(.props.name)"
      val expected = JMethodCall(
        "MyClass",
        "myMethod",
        Chunk(JPath(JsltNode("props"), JsltNode("name")))
      )
      val result = Jslt.parse(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse method with multiple args") {
      val input =
        "MyClass:myMethod(.props.name, .props.age)"
      val expected = JMethodCall(
        "MyClass",
        "myMethod",
        Chunk(
          JPath(JsltNode("props"), JsltNode("name")),
          JPath(JsltNode("props"), JsltNode("age")),
        )
      )
      val result = Jslt.parse(input)
      assert(result)(isRight(equalTo(expected)))
    },
    test("parse method name in an object"){
      val input = """{ "details": MyClass:myMethod(.props.name, .props.age) }"""
      val expectedMethodCall = JMethodCall(
        "MyClass",
        "myMethod",
        Chunk(
          JPath(JsltNode("props"), JsltNode("name")),
          JPath(JsltNode("props"), JsltNode("age")),
        )
      )
      val expected = JObject(Map(
        "details" -> expectedMethodCall
      ))
      val result = Jslt.parse(input)
      assert(result)(isRight(equalTo(expected)))
    }
  )
}
