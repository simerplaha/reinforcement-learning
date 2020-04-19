package tictactoe

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
 * Implements logic that is required for a player.
 */
sealed trait Player
object Player {

  private val defaultProbability: Double = 0.5
  private val learningRate: Double = 0.1
  private val winProbability = 1.0
  private val lossProbability = 0.0
  private val explorer = Random.javaRandomToRandom(new java.util.Random())

  /**
   * @return None if the place is already taken else returns the next board with move placed.
   */
  def playHuman(row: Int, col: Int, move: Move, board: Board.Active): Option[Board] =
    if (board.matrix.get(row, col).isEmpty)
      Some(Board.place(row, col, move, board))
    else
      None

  /**
   * Invokes the bot to play its move. This will find the optimal move to play.
   */
  def playBot(board: Board.Active, player: Player.Bot, exploreRate: Double): Board = {
    val possibleMoves = getPossibleMoves(board, player.move)

    //should explore or not
    val explore = explorer.nextDouble() <= exploreRate

    val nextBoard =
      if (explore)
        Random.shuffle(possibleMoves).head
      else
        optimalMove(possibleMoves, player)

    nextBoard match {
      case _: Board.Active =>
        //if the game is still going store the current play in the bot's state
        //which is used at the end of the game to update the bot's knowledge.
        player.currentPlay += CurrentPlay(nextBoard, !explore)

      case won: Board.Won =>
        //if this bot won, then update the knowledge.
        Player.wonGame(won, player)

      case draw: Board.Draw =>
      //losses are updated by the Game itself.
    }

    nextBoard
  }

  /**
   * Returns all the possible moves the bot can play given the current state of the board.
   */
  def getPossibleMoves(board: Board.Active, move: Move): List[Board] = {
    val possibleMoves = ListBuffer.empty[Board]

    board.matrix.foreachNone {
      case (row, col) =>
        possibleMoves += Board.place(row, col, move, board)
    }

    possibleMoves.to(List)
  }

  /**
   * Given all the moves the bot can play this function selects the best move to play from the bot's knowledge bank.
   */
  def optimalMove(possibleMoves: List[Board], bot: Bot): Board = {
    val optimalMoves =
      possibleMoves
        .map {
          board =>
            val probability = bot.knowledge.getOrElse(board.matrix, defaultProbability)
            (board, probability)
        }.sortBy(_._2)

    optimalMoves.lastOption match {
      case Some((board, _)) =>
        board

      case None =>
        Random.shuffle(possibleMoves).head
    }
  }

  /**
   * Updates the knowledge based on the formula in the book "Reinforcement learning An Introduction".
   *
   * TODO - Needs review.
   */
  def updateKnowledge(board: Board.Over, bot: Bot, winOrLossRating: Double) = {
    bot.currentPlay.reverse.foldLeft(winOrLossRating) {
      case (laterProbability, play) =>
        val existingProbability = bot.knowledge.getOrElse(play.board.matrix, defaultProbability)
        val newLaterProbability = existingProbability + ((laterProbability - existingProbability) * learningRate)

        bot.knowledge.put(play.board.matrix, newLaterProbability)
        newLaterProbability
    }

    bot.currentPlay.clear()
    //last move is always either win or loss so it should be 1.0 or 0.0
    bot.knowledge.put(board.matrix, winOrLossRating)
  }

  def wonGame(board: Board.Won, bot: Bot): Unit =
    updateKnowledge(board, bot, winProbability)

  def lostGame(board: Board.Over, bot: Bot): Unit =
    updateKnowledge(board, bot, lossProbability)

  /**
   * Stores the current play information in the game.
   */
  case class CurrentPlay(board: Board, greedyPlay: Boolean) {
    def explorePlay = !greedyPlay
  }

  object Bot {
    def apply(move: Move): Bot =
      new Bot(move, ListBuffer.empty, mutable.Map.empty)
  }

  case class Bot(move: Move,
                 currentPlay: ListBuffer[CurrentPlay],
                 knowledge: mutable.Map[Matrix[Move], Double]) extends Player
}
