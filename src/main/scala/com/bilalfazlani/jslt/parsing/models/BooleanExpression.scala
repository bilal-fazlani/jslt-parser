package com.bilalfazlani.jslt.parsing.models

import com.bilalfazlani.jslt.parsing.models.Jslt.JPath

sealed trait BinaryOperand

object BinaryOperand {
  case object GreaterThan extends BinaryOperand

  case object LessThan extends BinaryOperand

  case object GreaterThanOrEqual extends BinaryOperand

  case object LessThanOrEqual extends BinaryOperand

  case object Equal extends BinaryOperand

  case object NotEqual extends BinaryOperand
}

sealed trait UnaryOperand

object UnaryOperand {

  case object NotNull extends UnaryOperand

  case object Exists extends UnaryOperand
}

sealed trait BooleanExpression

object BooleanExpression {

  case class Condition(left: Jslt, operand: BinaryOperand, right: Jslt) extends BooleanExpression

  case class UnaryCondition(operand: UnaryOperand, right: Jslt) extends BooleanExpression

  case class And(left: BooleanExpression, right: BooleanExpression) extends BooleanExpression

  case class Or(left: BooleanExpression, right: BooleanExpression) extends BooleanExpression

  case class Not(expression: BooleanExpression) extends BooleanExpression

  case class BooleanExtractor(path: JPath) extends BooleanExpression

  case class JPathExpression(path: JPath) extends BooleanExpression
}

