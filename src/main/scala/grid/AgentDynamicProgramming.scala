package grid

import grid.Grid.{Action, World}
import util.Matrix

object AgentDynamicProgramming {

  /**
   * Action-value representation of the Grid World that the Agent builds
   * where each cell value is the probability of - how good it is to be
   * in that state.
   */
  case class ActionValues private(values: Matrix[Double]) {

    def cellValue(row: Int, col: Int): Option[Double] =
      Some(values(row, col))

    //return the value of the left cell at this row and value
    def moveLeft(row: Int, col: Int): Option[Action.Left[Double]] =
      if (col == 0)
        None
      else
        Some(Action.Left(values(row, col - 1)))

    //return the value of the right cell at this row and value
    def moveRight(row: Int, col: Int): Option[Action.Right[Double]] =
      if (col == values.cols - 1)
        None
      else
        Some(Action.Right(values(row, col + 1)))

    //return the value of the up cell at this row and value
    def moveUp(row: Int, col: Int): Option[Action.Up[Double]] =
      if (row == 0)
        None
      else
        Some(Action.Up(values(row - 1, col)))

    //return the value of the down cell at this row and value
    def moveDown(row: Int, col: Int): Option[Action.Down[Double]] =
      if (row == values.rows - 1)
        None
      else
        Some(Action.Down(values(row + 1, col)))

    //return all cells surrounding this row and col.
    def allActions(row: Int, col: Int): Seq[Option[Action[Double]]] =
      Seq(
        moveLeft(row, col),
        moveRight(row, col),
        moveUp(row, col),
        moveDown(row, col)
      )

    //rounding up decimals
    private def round(value: Double, scale: Int): Double =
      BigDecimal(value).setScale(scale, BigDecimal.RoundingMode.HALF_UP).toDouble

    //updates by mutating this object
    def update(row: Int, col: Int, value: Double): Unit =
      values.update(row, col, round(value, 2))
  }

  //fetches the values of the cell surrounding this cell to calculate the value of this cell.
  private def calculateActionValue(row: Int, col: Int, actionValues: ActionValues)(implicit world: World): Double = {
    val allActions = actionValues.allActions(row, col).flatten
    val cellReward = world.reward(row, col)
    val average = allActions.map(_.value).sum / allActions.size
    cellReward + average
  }

  private def updateActionValues(row: Int, col: Int, actionValues: ActionValues)(implicit world: World): Unit =
    world.matrix.get(row, col) match {
      case Grid.Terminal =>
      //terminal does not require mutation.

      case Grid.Cell =>
        val actionValue = calculateActionValue(row, col, actionValues)
        actionValues.update(row, col, actionValue)
    }

  def exploreLookAhead(actionValues: ActionValues)(implicit world: World): Unit =
    actionValues.values.foreachIndex {
      case (row, col) =>
        updateActionValues(row, col, actionValues)
    }

  //start exploring.
  def explore(actionValues: ActionValues, iterations: Int)(implicit world: World): Unit =
    (1 to iterations) foreach {
      _ =>
        exploreLookAhead(actionValues)
        println(actionValues.values.toString() + "\n")
    }

  //Uses values to build optimal direction matrix.
  def directionsMatrix(actionValues: ActionValues)(implicit world: World): Matrix[Option[Action[Double]]] =
    Matrix.fill(actionValues.values.rows, actionValues.values.cols) {
      case (row, col) =>
        world.matrix.get(row, col) match {
          case Grid.Terminal =>
            None

          case Grid.Cell =>
            val optimalDirection = actionValues.allActions(row, col).flatten.max
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

    val actionValues = ActionValues(Matrix.fill(world.rows, world.cols, 0.0))

    explore(
      actionValues = actionValues,
      iterations = 100
    )

    val directionMatrixString =
      directionsMatrix(actionValues)
        //if None then icon is home.
        .toStringCustom(_.map(_.icon).getOrElse("⌂"))

    println(directionMatrixString)
  }
}
