package util

import scala.reflect.ClassTag

object Matrix {
  def create[T: ClassTag](rows: Int, cols: Int)(filler: (Int, Int) => T): Matrix[T] =
    Matrix[T](
      rows = rows,
      cols = cols,
      elems = Array.tabulate(rows, cols)(filler)
    )

  def create[T: ClassTag](rows: Int, cols: Int, default: => T): Matrix[T] =
    Matrix[T](
      rows = rows,
      cols = cols,
      elems = Array.fill(rows)(Array.fill(cols)(default))
    )
}

case class Matrix[T: ClassTag](rows: Int,
                               cols: Int,
                               private val elems: Array[Array[T]]) {

  def toSeq: Seq[Seq[T]] =
    elems.map(_.to(Seq)).to(Seq)

  def updateCopy(row: Int, col: Int, value: T): Matrix[T] =
    Matrix(rows, cols, elems.updated(row, elems(row).updated(col, value)))

  def update(row: Int, col: Int, value: T): Unit =
    elems(row)(col) = value

  def row(pos: Int): Seq[T] =
    elems(pos)

  def apply(row: Int, col: Int): T =
    get(row, col)

  def get(row: Int, col: Int): T =
    elems(row)(col)

  def map[B: ClassTag](f: Seq[T] => B): Seq[B] =
    elems.map(array => f(array))

  def foreach(f: (Int, Int) => Unit): Unit =
    (0 until rows) foreach {
      row =>
        (0 until cols) foreach {
          col =>
            f(row, col)
        }
    }

  def foreachValue(f: (Int, Int, T) => Unit): Unit =
    (0 until rows) foreach {
      row =>
        (0 until cols) foreach {
          col =>
            f(row, col, get(row, col))
        }
    }

  def foldLeft[B](initial: B)(f: (B, (Int, Int, T)) => B): B = {
    var value = initial
    foreachValue {
      case (row, col, move) =>
        value = f(value, (row, col, move))
    }
    value
  }

  def exists(f: T => Boolean): Boolean = {
    var row = 0
    var col = 0
    while (row < rows) {
      while (col < cols) {
        if (f(get(row, col)))
          return true
        else
          col += 1
      }
      row += 1
      col = 0
    }

    false
  }

  override def toString(): String =
    toStringCustom((_, _, value) => value.toString)

  def toStringCustom(toString: T => String): String =
    toStringCustom((_, _, value) => toString(value))

  /**
   * Given the row, col and the value returns a custom String to insert in the matrix.
   */
  def toStringCustom(toString: (Int, Int, T) => String): String = {
    val maxLength =
      this.foldLeft(0) {
        case (size, (row, col, value)) =>
          size max toString(row, col, value).length
      }

    (0 until rows).foldLeft("") {
      case (string, row) =>
        var colString = string

        if (row != 0) colString += "\n"
        colString += "["

        colString =
          (0 until cols).foldLeft(colString) {
            case (string, col) =>
              val value = get(row, col)
              val valueString = toString(row, col, value)
              val empty = maxLength - valueString.length()

              val tailSpace = (1 to empty).map(_ => " ").mkString

              val comma =
                if (col == cols - 1)
                  tailSpace
                else
                  ", " + tailSpace

              string + valueString + comma
          }

        colString + "]"
    }
  }
}