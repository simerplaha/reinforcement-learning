package util

import scala.reflect.ClassTag

object Matrix {
  def apply[T: ClassTag](rows: Array[Array[T]]): Matrix[T] =
    apply(rows: _*)

  def apply[T: ClassTag](rows: Array[T]*): Matrix[T] =
    new Matrix[T](
      rows = rows.length,
      cols = rows.head.length,
      array = rows.toArray
    )

  def fill[T: ClassTag](rows: Int, cols: Int)(filler: (Int, Int) => T): Matrix[T] =
    new Matrix[T](
      rows = rows,
      cols = cols,
      array = Array.tabulate(rows, cols)(filler)
    )

  def fill[T: ClassTag](rows: Int, cols: Int, default: => T): Matrix[T] =
    new Matrix[T](
      rows = rows,
      cols = cols,
      array = Array.fill(rows)(Array.fill(cols)(default))
    )

  implicit class VectorImplicits[T](array: Array[T]) {
    @inline def *[B >: T : ClassTag](b: Array[B])(implicit numeric: Numeric[B]): B = {
      import numeric._
      array.zip(b).foldLeft(numeric.zero) {
        case (sum, (a, b)) =>
          sum + (a * b)
      }
    }
  }
}


case class Matrix[T: ClassTag](rows: Int,
                               cols: Int,
                               private val array: Array[Array[T]]) {

  import util.Matrix._

  val shape: String = s"${rows}x$cols"

  def isVector: Boolean =
    rows == 1 || cols == 1

  def toSeq: Seq[Seq[T]] =
    array.map(_.to(Seq)).to(Seq)

  /**
   * @return All rows in the [[Matrix]]
   */
  def rowsArray: Array[Array[T]] =
    array

  /**
   * @return Iterators for rows
   */
  def rowsIterator: Iterator[Array[T]] =
    rowsArray.iterator

  /**
   * @return All colums in the Matrix
   */
  def columnsArray: Array[Array[T]] =
    (0 until cols).map {
      col =>
        (0 until rows).map {
          row =>
            get(row, col)
        }.to(Array)
    }.to(Array)

  /**
   * @return Column at at index.
   */
  def column(col: Int): Array[T] =
    (0 until rows).map {
      row =>
        get(row, col)
    }.to(Array)

  /**
   * @return Row at at index.
   */
  def row(row: Int): Array[T] =
    array(row)

  def columnsIterator() =
    columnsArray.iterator

  /**
   * @return A new matrix with the updated value.
   */
  def updateCopy(row: Int, col: Int, value: T): Matrix[T] =
    Matrix(rows, cols, array.updated(row, array(row).updated(col, value)))

  /**
   * Updates/mutates the value in this matrix.
   */
  def update(row: Int, col: Int, value: T): Unit =
    array(row)(col) = value

  def apply(row: Int, col: Int): T =
    get(row, col)

  def get(row: Int, col: Int): T =
    array(row)(col)

  /**
   * Applies the input function to the indexes (row, col)
   */
  def foreachIndex(f: (Int, Int) => Unit): Unit =
    (0 until rows) foreach {
      row =>
        (0 until cols) foreach {
          col =>
            f(row, col)
        }
    }

  /**
   * Applies the input function to the indexes and the value (row, col, value)
   */
  def foreachValue(f: (Int, Int, T) => Unit): Unit =
    (0 until rows) foreach {
      row =>
        (0 until cols) foreach {
          col =>
            f(row, col, get(row, col))
        }
    }

  def foldLeftIndexValue[B](initial: B)(f: (B, (Int, Int, T)) => B): B = {
    var value = initial
    foreachValue {
      case (row, col, move) =>
        value = f(value, (row, col, move))
    }
    value
  }

  private def assertMultiplication(y: Matrix[_]): Unit =
    assert(cols == y.rows, s"Cannot multiple - Matrix x has $cols columns whereas matrix y has ${y.rows} rows. Columns of X != Rows of Y")

  //inverts the matrix.
  def transpose: Matrix[T] =
    Matrix(columnsArray)

  private def vectorProduct[B >: T : ClassTag](y: Matrix[B])(implicit numeric: Numeric[B]): Matrix[B] = {
    import numeric._

    val result =
      array.zip(y.array).map {
        case (left, right) =>
          left.zip(right).map {
            case (left, right) =>
              left * right
          }
      }

    Matrix(result)
  }

  def *[B >: T : ClassTag](y: Matrix[B])(implicit numeric: Numeric[B]): Matrix[B] =
    if (this.isVector && y.isVector && rows == y.rows && cols == y.cols) {
      //special treatment for vectors.
      vectorProduct(y)
    } else {
      assertMultiplication(y)

      val result =
        array.map {
          row =>
            y.columnsArray map {
              yColumn =>
                row * yColumn
            }
        }

      Matrix(result)
    }

  def *[B >: T : ClassTag](scalar: B)(implicit numeric: Numeric[B]): Matrix[B] = {
    import numeric._
    val result =
      array map {
        array =>
          array map {
            element =>
              scalar * element
          }
      }

    Matrix(result)
  }

  def +[B >: T : ClassTag](y: Matrix[B])(implicit numeric: Numeric[B]): Matrix[B] =
    addOrSubtract[B](y, (left, right) => numeric.plus(left, right))

  def -[B >: T : ClassTag](y: Matrix[B])(implicit numeric: Numeric[B]): Matrix[B] =
    addOrSubtract[B](y, (left, right) => numeric.minus(left, right))

  private def assertAddOrSubtract(y: Matrix[_]): Unit = {
    assert(cols == y.cols, s"Cannot perform operation! Matrix x has $cols columns whereas matrix y has ${y.cols} cols. Columns should be same.")
    assert(rows == y.rows, s"Cannot perform operation! Matrix x has $rows rows whereas matrix y has ${y.rows} rows. Rows should be same.")
  }

  private def addOrSubtract[B >: T : ClassTag](y: Matrix[B], op: (T, B) => B): Matrix[B] = {
    assertAddOrSubtract(y)

    val result =
      array.zip(y.array) map {
        case (left, right) =>
          left.zip(right) map {
            case (left, right) =>
              op(left, right)
          }
      }

    Matrix(result)
  }

  private def mapArray[B: ClassTag](f: T => B): Array[Array[B]] =
    array map {
      array =>
        array map {
          element =>
            f(element)
        }
    }

  def map[B: ClassTag](f: T => B): Matrix[B] =
    Matrix(mapArray(f))

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

  def iterator: Iterator[T] =
    new Iterator[T] {
      var row = 0
      var col = 0

      private def prepareForNextRead() =
        if (col == cols - 1) {
          row += 1
          col = 0
        } else {
          col += 1
        }

      override def hasNext: Boolean =
        row < rows && col < cols

      override def next(): T = {
        val item = get(row, col)
        prepareForNextRead()
        item
      }
    }

  override def toString(): String =
    toStringCustom((_, _, value) => value.toString)

  def toStringCustom(toString: T => String): String =
    toStringCustom((_, _, value) => toString(value))

  /**
   * Given the row, col and the value returns a custom String to insert in the matrix.
   *
   * Pretty print for this matrix.
   */
  def toStringCustom(toString: (Int, Int, T) => String): String = {
    val maxLength =
      this.foldLeftIndexValue(0) {
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