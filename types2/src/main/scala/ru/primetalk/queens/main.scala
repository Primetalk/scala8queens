package ru.primetalk.queens

import scala.compiletime.ops.any.{==}
import scala.compiletime.ops.int.{S, Abs, -, +}
import scala.compiletime.ops.boolean.*

type If[c <: Boolean, t, f] = c match
  case true  => t
  case false => f

object ᚾ

type Nil = ᚾ.type
type ::[x, xs] = (x, xs)

/** Fold list using op, and zero z. */
type FoldLeft[z, op[_, _], list] = list match
  case x :: xs => FoldLeft[op[z, x], op, xs]
  case Nil     => z

type FoldLeftBoolean[z <: Boolean, op[
    _ <: Boolean,
    _ <: Boolean
] <: Boolean, list] <: Boolean = list match
  case x :: xs => FoldLeftBoolean[op[z, x], op, xs]
  case Nil     => z

type Map[f[arg], list] = list match
  case x :: xs => f[x] :: Map[f, xs]
  case Nil     => Nil
type MapInt[f[arg <: Int], list] = list match
  case x :: xs => f[x] :: MapInt[f, xs]
  case Nil     => Nil

/** Filter list elements. */
type Filter[p[_] <: Boolean, list] = list match
  case x :: xs => If[p[x], x :: Filter[p, xs], Filter[p, xs]]
  case Nil     => Nil

type Exists[p[_] <: Boolean, list] =
  FoldLeftBoolean[false, [previous <: Boolean,
  next] =>> previous || p[next], list]
// type AnyTrue[list] = FoldLeftBoolean[false, ||, list]

type RangeFromZeroTo0[n <: Int, xs] = n match
  case 0     => 0 :: xs
  case S[n1] => RangeFromZeroTo0[n1, n :: xs]
type RangeFromZeroTo[n <: Int] = RangeFromZeroTo0[n, Nil]

type Queen[x <: Int, y <: Int] = (x, y)

type RowOfQueens[cols, row <: Int] =
  MapInt[[col <: Int] =>> Queen[row, col], cols]

type Threatens[q1, q2] <: Boolean =
  (q1, q2) match
    case (Queen[ax, ay], Queen[bx, by]) =>
      ax == bx || ay == by || Abs[ax - bx] == Abs[ay - by]

type Safe[placedQueens, queen] =
  ![Exists[[placedQueen] =>> Threatens[placedQueen, queen], placedQueens]]

type FilterSafeQueens[candidates, placedQueens] =
  Filter[[candidate] =>> Safe[placedQueens, candidate], candidates]

type Next[row <: Int, placedQueens] =
  FilterSafeQueens[RowOfQueens[RangeFromZeroTo[N], row], placedQueens]

type SolveNextRow[row <: Int, placedQueens] =
  Solve[Next[row + 1, placedQueens], row + 1, placedQueens]

/** Solves 8-queen problem. candidates - the list of queens that are not
  * threatened by any of placedQueens. placedQueens - queens placed so far, row
  * \- index of the current row.
  */
type Solve[candidates, row <: Int, placedQueens] =
  If[
    row == N,
    candidates match
      case x :: _ => x :: placedQueens // return the solution
      case _      => Nil // no solution found
    ,
    candidates match
      case x :: xs =>
        SolveNextRow[row, x :: placedQueens] match
          case Nil => // no solution found with this assumption
            Solve[xs, row, placedQueens]
          case y :: ys => // there is a solution
            y :: ys // SolveNextRow[row, x :: placedQueens]
      case _ =>
        Nil
  ]

type N = 6
type Solution = Solve[Next[0, Nil], 0, Nil]

@main
def main(): Unit =
  // println(summon[Render[::[1,Nil]]].render)
  // println(summon[Render[1-3]].render)
  // // println(summon[Render[AbsDiff[1, 2]]].render)
  // // println(summon[Render[IntEquals[AbsDiff[1, 2], AbsDiff[2, 1]]]].render)
  // println(summon[Render[Threatens[Queen[0, 1], Queen[3, 3]]]].render)
  // println(summon[ToStringList[::[1, Nil]]].toStringList)
  // println(summon[ToStringList[RangeFromZeroTo[0]]].toStringList)
  // println(summon[Render[RangeFromZeroTo[1]]].render)
  // println(summon[Render[Next[0, Nil]]].render)
  println(summon[Render[Solution]].render)
