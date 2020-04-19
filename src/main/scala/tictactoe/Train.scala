package tictactoe

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * Trains the input player2 Bot by compete against another Bot.
 */
object Train {

  def apply(games: Int, exploreRate: Double, player2: Player.Bot): Unit = {
    println("Training ...")
    val player1 = Player.Bot(Move.X, ListBuffer.empty, mutable.Map.empty)

    //winners update their state after their last move but looser needs to be informed.
    def informLoser(board: Board.Over, player: Player) =
      if (player == player1)
        Player.lostGame(board, player1)
      else
        Player.lostGame(board, player1)

    @tailrec
    def play(board: Board.Active, player: Player.Bot): Board.Over =
      Player.playBot(board, player, exploreRate) match {
        case board: Board.Active =>
          if (player == player1)
            play(board, player1)
          else
            play(board, player1)

        case over: Board.Over =>
          informLoser(over, player)
          over
      }

    (1 to games) foreach {
      _ =>
        play(Board.create(), player1)
    }
  }
}
