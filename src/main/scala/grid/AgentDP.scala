package grid

import grid.Grid.{Action, World}
import util.Matrix

object AgentDP {

  /**
   * Action-value representation of the Grid World that the Agent builds
   * where each cell value is the probability of - how good it is to be
   * in that state.
   */
  case class ActionValues private(values: Matrix[Double]) {

    def cellValue(row: Int, col: Int): Option[Double] =
      Some(values(row, col))

    //return the value of the left cell at this row and value
    def leftCellValue(row: Int, col: Int): Option[Action.Left[Double]] =
      if (col == 0)
        None
      else
        Some(Action.Left(values(row, col - 1)))

    //return the value of the right cell at this row and value
    def rightCellValue(row: Int, col: Int): Option[Action.Right[Double]] =
      if (col == values.cols - 1)
        None
      else
        Some(Action.Right(values(row, col + 1)))

    //return the value of the up cell at this row and value
    def upCellValue(row: Int, col: Int): Option[Action.Up[Double]] =
      if (row == 0)
        None
      else
        Some(Action.Up(values(row - 1, col)))

    //return the value of the down cell at this row and value
    def downCellValue(row: Int, col: Int): Option[Action.Down[Double]] =
      if (row == values.rows - 1)
        None
      else
        Some(Action.Down(values(row + 1, col)))

    //return all cells surrounding this row and col.
    def actionValues(row: Int, col: Int): Seq[Option[Action[Double]]] =
      Seq(
        leftCellValue(row, col),
        rightCellValue(row, col),
        upCellValue(row, col),
        downCellValue(row, col)
      )

    //rounding up decimals
    private def round(value: Double): Double =
      BigDecimal(value).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble

    //updates by mutating this object
    def updateMutable(row: Int, col: Int, value: Double): Unit =
      values.update(row, col, round(value))

    //updates and returns a copy
    def updateImmutable(row: Int, col: Int, value: Double): Matrix[Double] =
      values.updateCopy(row, col, round(value))
  }

  //fetches the values of the cell surrounding this cell to calculate the value of this cell.
  private def calculateActionValue(row: Int, col: Int, agent: ActionValues)(implicit world: World): Double = {
    val allActions = agent.actionValues(row, col).flatten
    val cellReward = world.reward(row, col)
    val average = allActions.map(_.value).sum / allActions.size
    cellReward + average
  }

  private def lookAheadMutateActionValue(row: Int, col: Int, agent: ActionValues)(implicit world: World): Unit =
    world.matrix.get(row, col) match {
      case Grid.Terminal =>
      //terminal does not require mutation.

      case Grid.Cell =>
        val actionValue = calculateActionValue(row, col, agent)
        agent.updateMutable(row, col, actionValue)
    }

  def exploreLookAhead(agent: ActionValues)(implicit world: World): Unit =
    agent.values.foreachIndex {
      case (row, col) =>
        lookAheadMutateActionValue(row, col, agent)
    }

  //start exploring.
  def explore(agent: ActionValues, iterations: Int)(implicit world: World): Unit =
    (1 to iterations) foreach {
      _ =>
        exploreLookAhead(agent)
        println(agent.values.toString() + "\n")
    }

  //Uses values to build optimal direction matrix.
  def directionsMatrix(agent: ActionValues)(implicit world: World): Matrix[Option[Action[Double]]] =
    Matrix.fill[Option[Action[Double]]](agent.values.rows, agent.values.cols) {
      case (row, col) =>
        world.matrix.get(row, col) match {
          case Grid.Terminal =>
            None

          case Grid.Cell =>
            val optimalDirection = agent.actionValues(row, col).flatten.max
            Some(optimalDirection)
        }
    }

  //start training
  def main(args: Array[String]): Unit = {
    implicit val world: Grid.World =
      Grid.World(
        rows = 11,
        cols = 11,
        terminals = Seq((0, 0), (10, 10))
      )

    val agent = ActionValues(Matrix.fill[Double](world.rows, world.cols, 0.0))

    explore(
      agent = agent,
      iterations = 100
    )

    val directionMatrixString =
      directionsMatrix(agent)
        //if None then icon is home.
        .toStringCustom(_.map(_.icon).getOrElse("⌂"))

    println(directionMatrixString)
  }
}
