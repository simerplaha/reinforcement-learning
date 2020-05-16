package grid

import util.Matrix

sealed trait Grid
object Grid {
  case object Terminal extends Grid
  case object Cell extends Grid

  sealed trait Action[T] {
    def value: T
    def icon: String
  }

  object Action {
    implicit val doubleOrdering = Ordering.by[Action[Double], Double](_.value)

    case class Left[T](value: T) extends Action[T] {
      def icon: String = "←"
    }

    case class Right[T](value: T) extends Action[T] {
      override def icon: String = "→"
    }

    case class Up[T](value: T) extends Action[T] {
      override def icon: String = "↑"
    }

    case class Down[T](value: T) extends Action[T] {
      override def icon: String = "↓"
    }
  }

  object World {
    def apply(rows: Int, cols: Int, terminals: Seq[(Int, Int)]): World = {
      val matrix =
        Matrix.fill[Grid](rows, cols) {
          case rowCol =>
            if (terminals.contains(rowCol))
              Grid.Terminal
            else
              Grid.Cell
        }

      World(matrix)
    }
  }

  case class World private(matrix: Matrix[Grid]) {

    def rows = matrix.rows

    def cols = matrix.cols

    def reward(row: Int, col: Int): Int =
      matrix.get(row, col) match {
        case Grid.Terminal =>
          0

        case Grid.Cell =>
          -1
      }
  }

  case class ActionValues private(values: Matrix[Double]) {

    def cellValue(row: Int, col: Int): Option[Double] =
      Some(values(row, col))

    def leftCellValue(row: Int, col: Int): Option[Action.Left[Double]] =
      if (col == 0)
        None
      else
        Some(Action.Left(values(row, col - 1)))

    def rightCellValue(row: Int, col: Int): Option[Action.Right[Double]] =
      if (col == values.cols - 1)
        None
      else
        Some(Action.Right(values(row, col + 1)))

    def upCellValue(row: Int, col: Int): Option[Action.Up[Double]] =
      if (row == 0)
        None
      else
        Some(Action.Up(values(row - 1, col)))

    def downCellValue(row: Int, col: Int): Option[Action.Down[Double]] =
      if (row == values.rows - 1)
        None
      else
        Some(Action.Down(values(row + 1, col)))

    def actionValues(row: Int, col: Int): Seq[Option[Action[Double]]] =
      Seq(
        leftCellValue(row, col),
        rightCellValue(row, col),
        upCellValue(row, col),
        downCellValue(row, col)
      )

    private def scale(value: Double): Double =
      BigDecimal(value).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble

    def updateMutable(row: Int, col: Int, value: Double): Unit =
      values.update(row, col, scale(value))

    def updateImmutable(row: Int, col: Int, value: Double): Matrix[Double] =
      values.updateCopy(row, col, scale(value))
  }
}
