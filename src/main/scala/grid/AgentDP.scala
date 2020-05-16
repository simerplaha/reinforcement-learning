package grid

import grid.Grid.{Action, ActionValues, World}
import util.Matrix

object AgentDP {

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

  def explore(agent: ActionValues, iterations: Int)(implicit world: World): Unit =
    (1 to iterations) foreach {
      _ =>
        exploreLookAhead(agent)
        println(agent.values.toString() + "\n")
    }

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
