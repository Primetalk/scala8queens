package ru.primetalk.queens.plain

def RangeFromZeroTo(n: Int, xs: List[Int] = Nil): List[Int] =
  if n == 0 then 0 :: xs
  else RangeFromZeroTo(n - 1, n :: xs)

case class Queen(x: Int, y: Int)

def RowOfQueens(cols: List[Int], row: Int) =
  cols.map(Queen(row, _))

def Threatens(q1: Queen, q2: Queen): Boolean =
  (q1, q2) match
    case (Queen(ax, ay), Queen(bx, by)) =>
      ax == bx || ay == by || math.abs(ax - bx) == math.abs(ay - by)

def Safe(placedQueens: List[Queen], queen: Queen): Boolean =
  !placedQueens.exists((placedQueen) => Threatens(placedQueen, queen))

def FilterSafeQueens(
    candidates: List[Queen],
    placedQueens: List[Queen]
): List[Queen] =
  candidates.filter((candidate) => Safe(placedQueens, candidate))

def Next(N: Int, row: Int, placedQueens: List[Queen]): List[Queen] =
  FilterSafeQueens(RowOfQueens(RangeFromZeroTo(N), row), placedQueens)

def SolveNextRow(N: Int, row: Int, placedQueens: List[Queen]): List[Queen] =
  Solve(N, Next(N, row + 1, placedQueens), row + 1, placedQueens)

/** Solves 8-queen problem. candidates - the list of queens that are not
  * threatened by any of placedQueens. placedQueens - queens placed so far, row
  * \- index of the current row.
  */
def Solve(
    N: Int,
    candidates: List[Queen],
    row: Int,
    placedQueens: List[Queen]
): List[Queen] =
  if row == N then
    candidates match
      case x :: _ => x :: placedQueens // return the solution
      case _      => Nil // no solution found
  else
    candidates match
      case x :: xs =>
        SolveNextRow(N, row, x :: placedQueens) match
          case Nil => // no solution found with this assumption
            Solve(N, xs, row, placedQueens)
          case y :: ys => // there is a solution
            y :: ys // SolveNextRow(row, x :: placedQueens)
      case _ =>
        Nil

def Solution(N: Int) = Solve(N, Next(N, 0, Nil), 0, Nil)

@main
def main(): Unit =
  println(s"${Solution(7)}")
