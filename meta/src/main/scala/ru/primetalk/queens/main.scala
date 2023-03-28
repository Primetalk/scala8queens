package ru.primetalk.queens

import scala.quoted.Expr
import scala.quoted.Quotes

import ru.primetalk.queens.MyMacros

inline def Solution(inline N: Int): List[plain.Queen] = ${
  MyMacros.SolutionM('N)
}

@main
def main(): Unit =
  val result: List[plain.Queen] = Solution(7)
  println(result)
