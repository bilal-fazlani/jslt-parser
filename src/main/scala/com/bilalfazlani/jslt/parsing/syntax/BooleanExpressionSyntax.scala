package com.bilalfazlani.jslt.parsing.syntax

import com.bilalfazlani.jslt.parsing.models.{
  BooleanExpression,
  ComparisonOperator,
  Jslt
}
import com.bilalfazlani.jslt.parsing.models.BooleanExpression._
import com.bilalfazlani.jslt.parsing.models.Jslt.JLiteral.JBoolean
import com.bilalfazlani.jslt.parsing.models.Jslt.JPath
import zio.Chunk
import zio.parser.Syntax._
import zio.parser.{StringErrSyntaxOps, Syntax, SyntaxOps}

trait BooleanTokenSyntax extends JsltParsingConstructs {
  this: JsltSyntax =>

  protected[parsing] sealed trait BooleanToken
  protected[parsing] sealed trait NonLeftRecursiveToken extends BooleanToken

  protected[parsing] case class BooleanValue(value: JBoolean)
      extends NonLeftRecursiveToken
  protected[parsing] case class JPathValue(value: JPath)
      extends NonLeftRecursiveToken

  protected[parsing] case class NotToken(expression: BooleanToken)
      extends NonLeftRecursiveToken

  protected[parsing] case class MethodCallToken(name: String, args: Chunk[Jslt])
      extends NonLeftRecursiveToken

  protected[parsing] case class LeftRecursiveToken(
      head: NonLeftRecursiveToken,
      tail: Chunk[Pair]
  ) extends BooleanToken

  protected[parsing] case class Pair(
      operator: BooleanOperator,
      operand: NonLeftRecursiveToken
  )

  protected[parsing] sealed trait BooleanOperator
  protected[parsing] case object AndToken extends BooleanOperator
  protected[parsing] case object OrToken extends BooleanOperator

  protected[parsing] sealed trait ComparisonOperatorToken

  object ComparisonOperatorToken {
    protected[parsing] case object Equal extends ComparisonOperatorToken
    protected[parsing] case object NotEqual extends ComparisonOperatorToken
  }

  protected[parsing] case class ComparisonToken(
      left: Jslt,
      operator: ComparisonOperatorToken,
      right: Jslt
  ) extends NonLeftRecursiveToken

  protected[parsing] lazy val booleanLiteralSyntax
      : Syntax[String, Char, Char, BooleanValue] =
    jBooleanSyntax.of[BooleanValue]

  protected[parsing] lazy val pathSyntax
      : Syntax[String, Char, Char, JPathValue] =
    jPathSyntax.of[JPathValue] ?? "jpath"

  protected[parsing] lazy val nonLeftRecursiveSyntax
      : Syntax[String, Char, Char, NonLeftRecursiveToken] =
    comparisonSyntax.widen[NonLeftRecursiveToken] |
      notSyntax.widen[NonLeftRecursiveToken] |
      methodCallSyntax.widen[NonLeftRecursiveToken] |
      pathSyntax.widen[NonLeftRecursiveToken] |
      booleanLiteralSyntax.widen[NonLeftRecursiveToken]

  protected[parsing] lazy val operatorSyntax
      : Syntax[String, Char, Char, BooleanOperator] =
    (string(" and ", AndToken).widen[BooleanOperator] |
      string(" or ", OrToken).widen[BooleanOperator]) ?? "boolean operator"

  protected[parsing] lazy val leftRecursiveSyntax
      : Syntax[String, Char, Char, LeftRecursiveToken] =
    (nonLeftRecursiveSyntax ~ (operatorSyntax ~ nonLeftRecursiveSyntax).repeat)
      .transform[LeftRecursiveToken](
        { case (head, tail) =>
          LeftRecursiveToken(head, tail.map((Pair.apply _).tupled))
        },
        { case LeftRecursiveToken(head, tail) =>
          (head, tail.map(p => (p.operator, p.operand)))
        }
      )

  protected[parsing] lazy val notSyntax: Syntax[String, Char, Char, NotToken] =
    (literal("not").unit("not") ~ optionalWhitespace ~ tokenSyntax.paren)
      .of[NotToken] ?? "not"

  protected[parsing] lazy val methodCallSyntax
      : Syntax[String, Char, Char, MethodCallToken] =
    ((alphaNumeric | acceptableSymbols).repeat.mkString ~ optionalWhitespace ~ jsltSyntax
      .separatedBy(comma)
      .paren).of[MethodCallToken] ?? "method call"

  // comparison operators
  private lazy val comparisonOperatorSyntax =
    string("==", ComparisonOperatorToken.Equal)
      .widen[ComparisonOperatorToken] ?? "== operator" |
      string("!=", ComparisonOperatorToken.NotEqual)
        .widen[ComparisonOperatorToken] ?? "!= operator"

  protected[parsing] lazy val comparisonSyntax
      : Syntax[String, Char, Char, ComparisonToken] =
    (jsltSyntax ~ requiredWhitespace ~ comparisonOperatorSyntax ~ requiredWhitespace ~ jsltSyntax)
      .optionalParen
      .of[ComparisonToken] ?? "comparison"

  protected[parsing] lazy val tokenSyntax
      : Syntax[String, Char, Char, BooleanToken] = {
    leftRecursiveSyntax.widen[BooleanToken] | nonLeftRecursiveSyntax.widen[BooleanToken]
  }
}

trait BooleanExpressionSyntax
    extends JsltParsingConstructs
    with BooleanTokenSyntax {
  this: JsltSyntax =>

  private def comparisonOperator(
      token: ComparisonOperatorToken
  ): ComparisonOperator = token match {
    case ComparisonOperatorToken.Equal    => ComparisonOperator.Equal
    case ComparisonOperatorToken.NotEqual => ComparisonOperator.NotEqual
  }

  private[parsing] def transformLeftToRight(
      token: BooleanToken
  ): BooleanExpression =
    token match {
      case BooleanValue(value) => BooleanLiteral(value)
      case JPathValue(value)   => JPathExpression(value)
      case ComparisonToken(left, op, right) =>
        Comparison(left, comparisonOperator(op), right)
      case NotToken(expression)        => Not(transformLeftToRight(expression))
      case MethodCallToken(name, args) => MethodCall(name, args)
      case LeftRecursiveToken(head, tail) =>
        tail.foldLeft[BooleanExpression](transformLeftToRight(head)) {
          case (lhs, Pair(AndToken, rhs)) => And(lhs, transformLeftToRight(rhs))
          case (lhs, Pair(OrToken, rhs))  => Or(lhs, transformLeftToRight(rhs))
        }
    }

  lazy val booleanExpression: Syntax[String, Char, Char, BooleanExpression] =
    tokenSyntax.transform(transformLeftToRight, x => ???)
}
