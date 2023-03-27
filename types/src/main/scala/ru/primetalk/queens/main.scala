package ru.primetalk.queens

object ᚾ
object ᛊ
object ᛚ
object ᛞ

type Nil = ᚾ.type
type Cons[x, xs] = (x, xs)

//type List[x] = Nil | Cons[x, List[x]]

type True = ᛊ.type
type False = ᛚ.type

type NewBoolean = True | False

type Not[b1] = b1 match
  case True  => False
  case False => True
  case _     => Nothing

type Or[b1, b2] = b1 match
  case True  => True
  case False => b2
  case _     => Nothing

type AnyTrue[list] = list match
  case Cons[x, xs] =>
    x match
      case True => True
      case _    => AnyTrue[xs]
  case _ => False

type Zero = ᛞ.type

type S[n] = (n, Unit)

type One = S[Zero]
type Two = S[One]
type Three = S[Two]
type Four = S[Three]
type Five = S[Four]
type Six = S[Five]
type Seven = S[Six]
type Eight = S[Seven]

//type Number = Zero | S[Number]

type Equals[a, b] = (a, b) match
  case (S[a1], S[b1]) =>
    Equals[a1, b1]
  case (Zero, Zero) => True
  case _            => False

type AbsDiff[a, b] = (a, b) match
  case (S[a1], S[b1]) =>
    AbsDiff[a1, b1]
  case (S[a1], Zero) =>
    S[a1]
  case (Zero, S[b1]) =>
    S[b1]
  case _ => Nothing

type RangeFromZeroTo0[n, xs] = n match
  case S[n1] => RangeFromZeroTo0[n1, Cons[n, xs]]
  case _     => Cons[Zero, xs]
type RangeFromZeroTo[n] = RangeFromZeroTo0[n, Nil]

type Queen[x, y] = (x, y)

type RowOfQueens[cols, row] =
  cols match
    case Cons[col, cols1] => Cons[Queen[row, col], RowOfQueens[cols1, row]]
    case _ =>
      cols

type Threatens[a, b] =
  (a, b) match
    case (Queen[ax, ay], Queen[bx, by]) =>
      Or[
        Or[
          Equals[ax, bx],
          Equals[ay, by],
        ],
        Equals[AbsDiff[ax, bx], AbsDiff[ay, by]]
      ]
    case _ => Nothing

type ThreateningQueens[placedQueens, queen] =
  placedQueens match
    case Cons[placedQueen, placedQueens1] =>
      Cons[
        Threatens[placedQueen, queen],
        ThreateningQueens[placedQueens1, queen]
      ]
    case _ =>
      Nil

type Safe[placedQueens, queen] =
  Not[AnyTrue[ThreateningQueens[placedQueens, queen]]]

type FilterSafeQueens[candidates, placedQueens] =
  candidates match
    case Cons[q, qs] =>
      Safe[placedQueens, q] match
        case True =>
          Cons[q, FilterSafeQueens[qs, placedQueens]]
        case _ =>
          FilterSafeQueens[qs, placedQueens]
    case _ => Nil

type Next[row, placedQueens] =
  FilterSafeQueens[RowOfQueens[RangeFromZeroTo[N], row], placedQueens]

type SolveNextRow[row, placedQueens] =
  Solve[Next[S[row], placedQueens], S[row], placedQueens]

type Solve[candidates, row, placedQueens] = Equals[row, N] match
  case True =>
    candidates match
      case Cons[x, _] =>
        Cons[x, placedQueens]
      case _ =>
        Nil
  case _ =>
    candidates match
      case Cons[x, xs] =>
        SolveNextRow[row, Cons[x, placedQueens]] match
          case Nil =>
            Solve[xs, row, placedQueens]
          case _ =>
            SolveNextRow[row, Cons[x, placedQueens]]
      case _ =>
        Nil

type N = Seven
type Solution = Solve[Next[Zero, Nil], Zero, Nil]

@main
def main(): Unit =
  println(summon[Render[Solution]].render)
