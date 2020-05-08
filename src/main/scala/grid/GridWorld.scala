package grid

import util.Matrix

object GridWorld extends App {

  val gridRows = 11
  val gridCols = 11
  val maxValueIterations = 100

  /**
   * Default terminals are top left, middle and bottom right.
   */
  val terminals: Seq[(Int, Int)] =
    Seq((0, 0), ((gridRows - 1) / 2, (gridCols - 1) / 2), (gridRows - 1, gridCols - 1))

  sealed trait Grid
  object Grid {
    case object Terminal extends Grid
    case object Cell extends Grid
  }

  sealed trait Action[T] {
    def value: T
    def icon: String
  }

  object Action {
    implicit val doubleOrdering = Ordering.by[Action[Double], Double](_.value)

    case class Left[T](value: T) extends Action[T] {
      def icon: String =
        "←"
    }

    case class Right[T](value: T) extends Action[T] {
      override def icon: String =
        "→"
    }

    case class Up[T](value: T) extends Action[T] {
      override def icon: String =
        "↑"
    }

    case class Down[T](value: T) extends Action[T] {
      override def icon: String =
        "↓"
    }
  }

  object Agent {
    private def calculateActionValue(agent: Agent, environment: Environment): Double = {
      val allActions = agent.actionValues().flatten
      val cellReward = environment.reward(agent.rowPos, agent.colPos)
      val average = allActions.map(_.value).sum / allActions.size
      cellReward + average
    }

    private def lookAheadUpdateActionValue(row: Int, col: Int, agent: Agent): Unit = {
      agent.move(row, col)
      environment.matrix.get(row, col) match {
        case Grid.Terminal =>

        case Grid.Cell =>
          val actionValue = Agent.calculateActionValue(agent, environment)
          agent.update(actionValue)
      }
    }

    def exploreLookAhead(agent: Agent): Unit =
      agent.matrix.foreach {
        case (row, col) =>
          lookAheadUpdateActionValue(row, col, agent)
      }

    def explore(agent: Agent, iterations: Int): Unit =
      (1 to iterations) foreach {
        _ =>
          exploreLookAhead(agent)
          println(agent.matrix.toString() + "\n")
      }

    def directionsMatrix(agent: Agent): Matrix[Option[Action[Double]]] =
      Matrix.create[Option[Action[Double]]](agent.matrix.rows, agent.matrix.cols) {
        case (row, col) =>
          agent.move(row, col)
          environment.matrix.get(row, col) match {
            case Grid.Terminal =>
              None

            case Grid.Cell =>
              Some(agent.optimalDirection())
          }
      }
  }

  case class Agent(private var rowPos: Int,
                   private var colPos: Int,
                   matrix: Matrix[Double] = Matrix.create[Double](gridRows, gridCols, 0.0)) {

    def move(row: Int, col: Int): Unit = {
      rowPos = row
      colPos = col
    }

    def cellValue(): Option[Double] =
      Some(matrix(rowPos, colPos))

    def leftCellValue(): Option[Action.Left[Double]] =
      if (colPos == 0)
        None
      else
        Some(Action.Left(matrix(rowPos, colPos - 1)))

    def rightCellValue(): Option[Action.Right[Double]] =
      if (colPos == gridCols - 1)
        None
      else
        Some(Action.Right(matrix(rowPos, colPos + 1)))

    def upCellValue(): Option[Action.Up[Double]] =
      if (rowPos == 0)
        None
      else
        Some(Action.Up(matrix(rowPos - 1, colPos)))

    def downCellValue(): Option[Action.Down[Double]] =
      if (rowPos == gridRows - 1)
        None
      else
        Some(Action.Down(matrix(rowPos + 1, colPos)))

    def actionValues(): Seq[Option[Action[Double]]] =
      Seq(leftCellValue(), rightCellValue(), upCellValue(), downCellValue())

    def update(value: Double): Unit = {
      val valueScaled = BigDecimal(value).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
      matrix.update(rowPos, colPos, valueScaled)
    }

    def optimalDirection(): Action[Double] =
      actionValues().flatten.max
  }

  case class Environment(matrix: Matrix[Grid]) {

    def reward(row: Int, col: Int): Int =
      matrix.get(row, col) match {
        case Grid.Terminal =>
          0

        case Grid.Cell =>
          -1
      }
  }

  def environmentMatrix =
    Matrix.create[Grid](gridRows, gridCols) {
      case rowCol =>
        if (terminals.contains(rowCol))
          Grid.Terminal
        else
          Grid.Cell
    }

  val environment = Environment(environmentMatrix)

  val agent = Agent(0, 0)

  Agent.explore(agent, maxValueIterations)

  val directionMatrixString =
    Agent
      .directionsMatrix(agent)
      //if None then icon is home.
      .toStringCustom(_.map(_.icon).getOrElse("⌂"))

  println(directionMatrixString)
}
