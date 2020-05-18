package grid

import util.Matrix

import scala.util.Random

sealed trait Grid
object Grid {
  case object Terminal extends Grid
  case object Cell extends Grid

  case class Position(row: Int, col: Int)

  sealed trait Action[T] {
    def value: T
    def icon: String
    def copyValue(value: T): Action[T]
  }

  object Action {
    implicit val doubleOrdering = Ordering.by[Action[Double], Double](_.value)

    /**
     * List all actions with the same value.
     */
    def allActions[T](value: T): Seq[Action[T]] =
      List(Left(value), Right(value), Up(value), Down(value))

    /**
     * Generate a random action with value.
     */
    def randomAction[T](value: T): Action[T] =
      Random.shuffle(allActions(value)).head

    case class Left[T](value: T) extends Action[T] {
      def icon: String = "←"

      override def copyValue(value: T): Action[T] =
        this.copy(value)
    }

    case class Right[T](value: T) extends Action[T] {
      override def icon: String = "→"

      override def copyValue(value: T): Action[T] =
        this.copy(value)
    }

    case class Up[T](value: T) extends Action[T] {
      override def icon: String = "↑"

      override def copyValue(value: T): Action[T] =
        this.copy(value)
    }

    case class Down[T](value: T) extends Action[T] {
      override def icon: String = "↓"

      override def copyValue(value: T): Action[T] =
        this.copy(value)
    }
  }

  object World {
    def apply(rows: Int,
              cols: Int,
              terminals: Seq[(Int, Int)],
              cellReward: Int = -1,
              terminateReward: Int = 0): World = {
      val matrix =
        Matrix.fill[Grid](rows, cols) {
          case rowCol =>
            if (terminals.contains(rowCol))
              Grid.Terminal
            else
              Grid.Cell
        }

      World(
        matrix = matrix,
        cellReward = cellReward,
        terminateReward = terminateReward
      )
    }
  }

  case class World private(matrix: Matrix[Grid],
                           cellReward: Int,
                           terminateReward: Int) {

    def rows = matrix.rows

    def cols = matrix.cols

    def get(row: Int, col: Int): Grid =
      matrix.get(row, col)

    def get(position: Position): Grid =
      matrix.get(position.row, position.col)

    def move(from: Position, action: Action[_]): Option[Position] =
      action match {
        case Action.Left(_) =>
          moveLeft(from)

        case Action.Right(_) =>
          moveRight(from)

        case Action.Up(_) =>
          moveUp(from)

        case Action.Down(_) =>
          moveDown(from)
      }

    def moveLeft(from: Position): Option[Position] =
      if (from.col == 0)
        None
      else
        Some(Position(from.row, from.col - 1))

    def moveRight(from: Position): Option[Position] =
      if (from.col == matrix.cols - 1)
        None
      else
        Some(Position(from.row, from.col + 1))

    def moveUp(from: Position): Option[Position] =
      if (from.row == 0)
        None
      else
        Some(Position(from.row - 1, from.col))

    def moveDown(from: Position): Option[Position] =
      if (from.row == matrix.rows - 1)
        None
      else
        Some(Position(from.row + 1, from.col))

    def reward(position: Position, action: Action[_]): Option[Int] =
      action match {
        case Action.Left(_) =>
          moveLeft(position).map(reward)

        case Action.Right(_) =>
          moveRight(position).map(reward)

        case Action.Up(_) =>
          moveUp(position).map(reward)

        case Action.Down(_) =>
          moveDown(position).map(reward)
      }

    def reward(position: Position): Int =
      reward(position.row, position.col)

    def reward(row: Int, col: Int): Int =
      matrix.get(row, col) match {
        case Grid.Terminal =>
          terminateReward

        case Grid.Cell =>
          cellReward
      }
  }
}
