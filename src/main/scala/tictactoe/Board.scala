package tictactoe

import util.Matrix

import scala.annotation.tailrec

/**
 * Board that manages where to the place the moves
 * and manages the state of the board.
 */
sealed trait Board {
  def matrix: Matrix[Option[Move]]

  def isOver: Boolean

  def isActive: Boolean = !isOver

  def prettyString: String =
    matrix.foldLeft("| ") {
      case (string, (row, col, move)) =>
        string + move.getOrElse("-") + " | " + {
          if (col == 2 && row < 2)
            "\n| "
          else
            ""
        }
    }
}

object Board {

  val MAX_BOARD_INDEX = 2

  def create(): Board.Active =
    Active(Matrix.create(3, 3, None))

  def place(row: Int, col: Int, move: Move, board: Board.Active): Board = {
    val updatedMoves = board.matrix.updateCopy(row, col, Some(move))
    val newBoard = Board.Active(updatedMoves)
    validateState(newBoard)
  }

  /**
   * After placing the move this check the valid state to return.
   */
  def validateState(board: Active): Board =
    getWinner(board) match {
      case Some(winner) =>
        Board.Won(board.matrix, winner)

      case None =>
        if (continue(board))
          board
        else
          Board.Draw(board.matrix)
    }

  /**
   * Checks if there are moves left on the board.
   */
  def continue(board: Board.Active): Boolean =
    board.matrix.exists(_.isEmpty)

  /**
   * Runs all checks on the board to see if there a winner.
   */
  def getWinner(board: Active): Option[Move] =
    getWinnerHorizontal(board) orElse
      getWinnerVertical(board) orElse
      getWinnerDiagonalRight(board) orElse
      getWinnerDiagonalLeft(board)

  def getWinnerHorizontal(board: Active): Option[Move] = {
    @tailrec
    def checkWinner(rowIndex: Int): Option[Move] =
      if (rowIndex > MAX_BOARD_INDEX) {
        None
      } else {
        val row: Seq[Option[Move]] = board.matrix.row(rowIndex)
        val head: Option[Move] = row.head
        if (head.isDefined && row.forall(_ == head))
          head
        else
          checkWinner(rowIndex + 1)
      }

    checkWinner(rowIndex = 0)
  }

  def getWinnerVertical(board: Active): Option[Move] = {
    @tailrec
    def checkWinner(colIndex: Int): Option[Move] =
      if (colIndex > MAX_BOARD_INDEX) {
        None
      } else {
        val col = board.matrix.map(_ (colIndex))
        val head: Option[Move] = col.head
        if (head.isDefined && col.forall(_ == head))
          head
        else
          checkWinner(colIndex + 1)
      }

    checkWinner(colIndex = 0)
  }

  def getWinnerDiagonalRight(board: Active): Option[Move] = {
    @tailrec
    def checkWinner(index: Int, previousMove: Option[Move]): Option[Move] =
      if (index > MAX_BOARD_INDEX)
        previousMove
      else
        board.matrix(index, index) match {
          case nextMove @ Some(_) =>
            if (index == 0)
              checkWinner(index + 1, nextMove)
            else if (previousMove == nextMove)
              checkWinner(index + 1, previousMove)
            else
              None

          case None =>
            None
        }

    checkWinner(index = 0, previousMove = None)
  }

  def getWinnerDiagonalLeft(board: Active): Option[Move] = {
    @tailrec
    def checkWinner(row: Int, col: Int, previousMove: Option[Move]): Option[Move] =
      if (row > MAX_BOARD_INDEX)
        previousMove
      else
        board.matrix(row, col) match {
          case nextMove @ Some(_) =>
            if (row == 0)
              checkWinner(row + 1, col - 1, nextMove)
            else if (previousMove == nextMove)
              checkWinner(row + 1, col - 1, previousMove)
            else
              None

          case None =>
            None
        }

    checkWinner(row = 0, col = 2, previousMove = None)
  }

  /**
   * Indicates that board is still in play state.
   */
  case class Active private(matrix: Matrix[Option[Move]]) extends Board {
    def isOver: Boolean = false
  }

  sealed trait Over extends Board {
    def isOver: Boolean = true
  }

  /**
   * Game over - There is a winner!
   */
  case class Won private(matrix: Matrix[Option[Move]],
                         winner: Move) extends Over

  /**
   * Game over - It's a draw!
   */
  case class Draw private(matrix: Matrix[Option[Move]]) extends Over
}
