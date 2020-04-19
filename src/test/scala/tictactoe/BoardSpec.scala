package tictactoe

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Random

class BoardSpec extends AnyWordSpec with Matchers {

  "Board" should {
    "place X and O" in {
      val board = Board.create()
      val board2 = Board.place(0, 1, Move.O, board).asInstanceOf[Board.Active]
      val board3 = Board.place(0, 2, Move.X, board2)

      board3.matrix.get(0, 0) shouldBe empty
      board3.matrix.get(0, 1) shouldBe Some(Move.O)
      board3.matrix.get(0, 2) shouldBe Some(Move.X)
    }

    "not declare winner" when {
      "inline" in {
        Board.validateState(Board.create()).isOver shouldBe false

        val board = Board.create()
        val board1 = Board.place(0, 0, Move.O, board).asInstanceOf[Board.Active]
        Board.place(0, 1, Move.O, board1).asInstanceOf[Board.Active]

      }

      "diagonal right" in {
        Board.validateState(Board.create()).isOver shouldBe false

        val board: Board.Active = Board.create()
        val board1 = Board.place(0, 0, Move.X, board).asInstanceOf[Board.Active]
        val board2 = Board.place(1, 1, Move.O, board1).asInstanceOf[Board.Active]
        Board.place(2, 2, Move.O, board2) shouldBe a[Board.Active]
      }

      "diagonal left" in {
        Board.validateState(Board.create()).isOver shouldBe false

        val board: Board.Active = Board.create()
        val board1 = Board.place(0, 2, Move.O, board).asInstanceOf[Board.Active]
        val board2 = Board.place(1, 1, Move.X, board1).asInstanceOf[Board.Active]
        Board.place(2, 0, Move.O, board2) shouldBe a[Board.Active]
      }
    }

    "declare winner" when {
      "inline" in {
        Board.validateState(Board.create()).isOver shouldBe false

        val board = Board.create()
        val board1 = Board.place(0, 0, Move.O, board).asInstanceOf[Board.Active]
        val board2 = Board.place(0, 1, Move.O, board1).asInstanceOf[Board.Active]
        val board3 = Board.place(0, 2, Move.O, board2).asInstanceOf[Board.Over]

        board3 shouldBe Board.Won(board3.matrix, Move.O)
      }

      "diagonal right" in {
        Board.validateState(Board.create()).isOver shouldBe false

        val board = Board.create()
        val board1 = Board.place(0, 0, Move.O, board).asInstanceOf[Board.Active]
        val board2 = Board.place(1, 1, Move.O, board1).asInstanceOf[Board.Active]
        val board3 = Board.place(2, 2, Move.O, board2).asInstanceOf[Board.Over]

        board3 shouldBe Board.Won(board3.matrix, Move.O)
      }

      "diagonal left" in {
        Board.validateState(Board.create()).isOver shouldBe false

        val board = Board.create()
        val board1 = Board.place(0, 2, Move.O, board).asInstanceOf[Board.Active]
        val board2 = Board.place(1, 1, Move.O, board1).asInstanceOf[Board.Active]
        val board3 = Board.place(2, 0, Move.O, board2).asInstanceOf[Board.Over]

        board3 shouldBe Board.Won(board3.matrix, Move.O)
      }
    }

  }
}
