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
}
