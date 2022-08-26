package com.bilalfazlani.jslt.parsing.syntax

import com.bilalfazlani.jslt.parsing.models.BooleanExpression
import com.bilalfazlani.jslt.parsing.models.BooleanExpression._
import com.bilalfazlani.jslt.parsing.models.Jslt.JLiteral.JBoolean
import com.bilalfazlani.jslt.parsing.models.Jslt.JPath
import zio.Chunk
import zio.parser.Syntax._
import zio.parser.{StringErrSyntaxOps, Syntax, SyntaxOps}

trait BooleanTokenSyntax extends JsltParsingConstructs {
  this: JsltSyntax =>

  protected[syntax] sealed trait BooleanToken
  protected[syntax] sealed trait NonLeftRecursiveToken extends BooleanToken

  protected[syntax] case class BooleanValue(value: JBoolean)
      extends NonLeftRecursiveToken
  protected[syntax] case class JPathValue(value: JPath)
      extends NonLeftRecursiveToken

  protected[syntax] case class NotToken(expression: BooleanToken)
      extends NonLeftRecursiveToken
  protected[syntax] case class ConverterToken(value: JPath)
      extends NonLeftRecursiveToken

  protected[syntax] case class LeftRecursiveToken(
      head: NonLeftRecursiveToken,
      tail: Chunk[Pair]
  ) extends BooleanToken

  protected[syntax] case class Pair(
      operator: BooleanOperator,
      operand: NonLeftRecursiveToken
  )

  protected[syntax] sealed trait BooleanOperator
  protected[syntax] case object AndToken extends BooleanOperator
  protected[syntax] case object OrToken extends BooleanOperator

  protected[syntax] lazy val valueSyntax
      : Syntax[String, Char, Char, BooleanValue] = jBooleanSyntax.of[BooleanValue]

  protected[syntax] lazy val pathSyntax
      : Syntax[String, Char, Char, JPathValue] =
    jPathSyntax.of[JPathValue] ?? "jpath"

  protected[syntax] lazy val nonLeftRecursiveSyntax
      : Syntax[String, Char, Char, NonLeftRecursiveToken] =
    pathSyntax.widen[NonLeftRecursiveToken] |
      valueSyntax.widen[NonLeftRecursiveToken] |
      converterSyntax.widen[NonLeftRecursiveToken] |
      notSyntax.widen[NonLeftRecursiveToken]

  protected[syntax] lazy val operatorSyntax
      : Syntax[String, Char, Char, BooleanOperator] =
    (string(" and ", AndToken).widen[BooleanOperator] |
      string(" or ", OrToken).widen[BooleanOperator]) ?? "boolean operator"

  protected[syntax] lazy val leftRecursiveSyntax
      : Syntax[String, Char, Char, LeftRecursiveToken] =
    (nonLeftRecursiveSyntax ~ (operatorSyntax ~ nonLeftRecursiveSyntax).repeat)
      .transform[LeftRecursiveToken](
        { case (head, tail) =>
          LeftRecursiveToken(head, tail.map(Pair.tupled))
        },
        { case LeftRecursiveToken(head, tail) =>
          (head, tail.map(p => (p.operator, p.operand)))
        }
      )

  protected[syntax] lazy val notSyntax: Syntax[String, Char, Char, NotToken] =
    (literal("not").unit("not") ~ optionalWhitespace ~ tokenSyntax.paren)
      .of[NotToken] ?? "not"

  protected[syntax] lazy val converterSyntax
      : Syntax[String, Char, Char, ConverterToken] =
    (literal("boolean").unit(
      "boolean"
    ) ~ optionalWhitespace ~ jPathSyntax.paren)
      .of[ConverterToken] ?? "boolean converter"

  protected[syntax] lazy val tokenSyntax
      : Syntax[String, Char, Char, BooleanToken] = {
    leftRecursiveSyntax.widen[BooleanToken] |
      nonLeftRecursiveSyntax.widen[BooleanToken]
  }
}

trait BooleanExpressionSyntax
    extends JsltParsingConstructs
    with BooleanTokenSyntax {
  this: JsltSyntax =>

  private[syntax] def transformLeftToRight(
      token: BooleanToken
  ): BooleanExpression =
    token match {
      case BooleanValue(value)   => BooleanLiteral(value)
      case JPathValue(value)     => JPathExpression(value)
      case NotToken(expression)  => Not(transformLeftToRight(expression))
      case ConverterToken(value) => BooleanConverter(value)
      case LeftRecursiveToken(head, tail) =>
        tail.foldLeft[BooleanExpression](transformLeftToRight(head)) {
          case (lhs, Pair(AndToken, rhs)) => And(lhs, transformLeftToRight(rhs))
          case (lhs, Pair(OrToken, rhs))  => Or(lhs, transformLeftToRight(rhs))
        }
    }

  lazy val booleanExpression: Syntax[String, Char, Char, BooleanExpression] =
    tokenSyntax.transform(transformLeftToRight, x => ???)
}