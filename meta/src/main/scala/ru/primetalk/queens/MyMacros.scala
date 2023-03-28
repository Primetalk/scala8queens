package ru.primetalk.queens

import scala.quoted.Expr
import scala.quoted.Quotes
import ru.primetalk.queens.plain.Queen
import scala.quoted.ToExpr

object MyMacros:
  given ToExpr[Queen] with
    def apply(queen: Queen)(using Quotes): Expr[Queen] =
      val Queen(x, y) = queen
      '{ Queen(${ Expr(x) }, ${ Expr(y) }) }

  def SolutionM(N: Expr[Int])(using Quotes): Expr[List[Queen]] =
    val solution = plain.Solution(N.valueOrAbort)
    Expr(solution)
