package ru.primetalk.queens

import scala.compiletime.ops.int.S
import scala.compiletime.ops.any.ToString
import scala.compiletime.constValue

import ru.primetalk.queens.{Nil, ::, Queen}

sealed trait ToInt[T <: Int]:
  def toInt: Int

given toInt[T <: Int: ValueOf]: ToInt[T] with
  inline def toInt: Int = valueOf[T]

// given sToInt[T <: Int](using t: ToInt[T]): ToInt[S[T]] with
//   def toInt: Int = t.toInt + 1

sealed trait Render[T]:
  def render: String

given renderInt[T <: Int](using t: ToInt[T]): Render[T] with
  def render: String = t.toInt.toString()

given renderQueen[x <: Int, y <: Int](using
    x: ToInt[x],
    y: ToInt[y]
): Render[Queen[x, y]] with
  def render: String = s"♕(${x.toInt}, ${y.toInt})"

sealed trait ToStringList[T]:
  def toStringList: List[String]

given ToStringList[Nil] with
  def toStringList: List[String] = scala.Nil

given consToList[x, xs](using
    x: Render[x],
    xs: ToStringList[xs]
): ToStringList[x :: xs] with
  def toStringList: List[String] = x.render :: xs.toStringList

given renderList[xs](using xs: ToStringList[xs]): Render[xs] with
  def render: String = xs.toStringList.mkString("(", ",", ")")

given renderTrue: Render[true] with
  def render: String = "t"

given renderFalse: Render[false] with
  def render: String = "f"
