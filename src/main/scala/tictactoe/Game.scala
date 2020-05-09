package tictactoe

import util.Util._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.StdIn._
import scala.util.Try

/**
 * Main class for the game. Runs training and initialises the game.
 */
object Game {

  def main(args: Array[String]): Unit = {
    println(
      """********
        |Training
        |********
        |""".stripMargin)

    print("Self play games (Hit enter for 1000000): ")
    val games = Try(readInt()) getOrElse 1000000

    print("Self play exploreRate (Enter between 0.0 to 1.0 or hit enter for 1.0): ")
    val trainExploreRate = Try(readDouble()) getOrElse 1.0

    val bot = Player.Bot(Move.O, ListBuffer.empty, mutable.Map.empty)

    Train(games, trainExploreRate, bot)
    println(s"Training data count: ${bot.knowledge.size}\n")

    print("Game play exploreRate (Hit enter for 0.2): ")
    val gameExploreRate = Try(readDouble()) getOrElse 0.2

    println(
      """
        |*********************************************
        |Game started - TODO - needs better prediction
        |*********************************************
        |""".stripMargin)

    println("You are X and the bot is O")
    val board = Board.create()

    playWithHuman(board, bot, gameExploreRate)
  }

  @tailrec
  def playWithHuman(board: Board, bot: Player.Bot, exploreRate: Double): Unit =
    board match {
      case board: Board.Active =>
        println(board.prettyString + "\n")

        print("Select Row and Column (1 to 3). For example: 1 2 or 2 2: ")

        val invalidInput = "Invalid input. Use '1 2' for row 1 and column 2"

        def readRowCol(): (Int, Int) =
          tryUntilSuccess {
            val rowCol = readLine().trim.split("\\s+?").map(_.toInt)
            assert(rowCol.length == 2, invalidInput)
            assert(rowCol.head >= 1 && rowCol.head <= 3, invalidInput)
            assert(rowCol.last >= 1 && rowCol.last <= 3, invalidInput)
            (rowCol.head - 1, rowCol.last - 1)
          }

        val (row, col) = readRowCol()

        Player.playHuman(row, col, Move.X, board) match {
          case Some(nextBoard) =>
            nextBoard match {
              case active: Board.Active =>
                val nextBoard = Player.playBot(active, bot, exploreRate)
                playWithHuman(nextBoard, bot, exploreRate)

              case over: Board.Over =>
                playWithHuman(over, bot, exploreRate)
            }

          case None =>
            println("This position is already taken.")
            playWithHuman(board, bot, exploreRate)
        }

      case board: Board.Over =>
        println(board.prettyString)

        board match {
          case Board.Won(_, winner) =>
            if (winner == Move.X) {
              println("You won!")
              Player.lostGame(board, bot)
            } else {
              println("You lost!")
            }

          case _: Board.Draw =>
            Player.lostGame(board, bot)
            println("Draw!")
        }

        println("\nNew Game!\n")
        playWithHuman(Board.create(), bot, exploreRate)
    }
}

case class Game(player1: Player,
                player2: Player,
                board: Board)
