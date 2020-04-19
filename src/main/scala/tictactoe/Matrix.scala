package tictactoe

object Matrix {

  def create[T](rows: Int, cols: Int): Matrix[T] =
    Matrix[T](
      rows = rows,
      cols = cols,
      elems = Seq.fill(rows)(Seq.fill(cols)(None))
    )
}

/**
 * Immutable matrix.
 *
 * TODO - Should use Breeze or another matrix library.
 */
case class Matrix[T](rows: Int,
                     cols: Int,
                     elems: Seq[Seq[Option[T]]]) {

  def update(row: Int, col: Int, value: T): Matrix[T] =
    Matrix(rows, cols, elems.updated(row, elems(row).updated(col, Some(value))))

  def row(pos: Int): Seq[Option[T]] =
    elems(pos)

  def apply(row: Int, col: Int): Option[T] =
    get(row, col)

  def get(row: Int, col: Int): Option[T] =
    elems(row)(col)

  def map[B](f: Seq[Option[T]] => B): Seq[B] =
    elems.map(f)

  def foreach(f: (Int, Int, Option[T]) => Unit): Unit =
    (0 until rows) foreach {
      row =>
        (0 until cols) foreach {
          col =>
            f(row, col, get(row, col))
        }
    }

  /**
   * Foreach on all slots in the [[elems]] that are [[None]].
   */
  def foreachNone(f: (Int, Int) => Unit): Unit =
    foreach {
      case (row, col, move) =>
        if (move.isEmpty)
          f(row, col)
    }

  def foldLeft[B](initial: B)(f: (B, (Int, Int, Option[T])) => B): B = {
    var value = initial
    foreach {
      case (row, col, move) =>
        value = f(value, (row, col, move))
    }
    value
  }

  def exists(f: Option[T] => Boolean): Boolean = {
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
}