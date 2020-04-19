package tictactoe

sealed trait Move
object Move {
  case object X extends Move {
    override def toString: String = "X"
  }

  case object O extends Move {
    override def toString: String =
      "O"
  }
}
