package com.bilalfazlani.jslt.parsing.models

import com.bilalfazlani.jslt.parsing.models.Jslt.JLiteral.JBoolean
import com.bilalfazlani.jslt.parsing.models.Jslt.JPath
import zio.Chunk
import com.bilalfazlani.jslt.parsing.syntax.JsltParsingConstructs
import com.bilalfazlani.jslt.parsing.syntax.JsltSyntax

sealed trait BinaryOperator

object BinaryOperator {
  case object GreaterThan extends BinaryOperator

  case object LessThan extends BinaryOperator

  case object GreaterThanOrEqual extends BinaryOperator

  case object LessThanOrEqual extends BinaryOperator

  case object Equal extends BinaryOperator

  case object NotEqual extends BinaryOperator
}

sealed trait BooleanExpression

object BooleanExpression {

  case class Condition(left: Jslt, operator: BinaryOperator, right: Jslt)
      extends BooleanExpression

  case class And(left: BooleanExpression, right: BooleanExpression)
      extends BooleanExpression

  case class Or(left: BooleanExpression, right: BooleanExpression)
      extends BooleanExpression

  case class Not(expression: BooleanExpression) extends BooleanExpression

  case class JPathExpression(path: JPath) extends BooleanExpression

  case class BooleanLiteral(value: JBoolean) extends BooleanExpression

  case class MethodCall(name: String, args: Chunk[Jslt]) extends BooleanExpression
  object MethodCall{
    def apply(name: String, arg: Jslt, args: Jslt*): MethodCall = MethodCall(name, Chunk(arg) ++ args)
  }
}