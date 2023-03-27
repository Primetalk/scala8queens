package ru.primetalk.queens

sealed trait ToInt[T]:
  def toInt: Int

given ToInt[Zero] with
  def toInt: Int = 0

given sToInt[T](using t: ToInt[T]): ToInt[S[T]] with
  def toInt: Int = t.toInt + 1

sealed trait Render[T]:
  def render: String

given renderInt[T](using t: ToInt[T]): Render[T] with
  def render: String = t.toInt.toString()

given renderQueen[x, y](using x: ToInt[x], y: ToInt[y]): Render[Queen[x, y]]
  with
  def render: String = s"♕(${x.toInt}, ${y.toInt})"

sealed trait ToStringList[T]:
  def toStringList: List[String]

given ToStringList[Nil] with
  def toStringList: List[String] = scala.Nil

given consToList[x, xs](using
    x: Render[x],
    xs: ToStringList[xs]
): ToStringList[Cons[x, xs]] with
  def toStringList: List[String] = x.render :: xs.toStringList

given renderList[xs](using xs: ToStringList[xs]): Render[xs] with
  def render: String = xs.toStringList.mkString("(", ",", ")")
