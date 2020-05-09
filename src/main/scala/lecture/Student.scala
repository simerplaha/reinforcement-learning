package lecture

import java.util.Random

/**
 * See tests [[StudentClassSpec]]
 */
sealed trait State {
  def reward: Int

  def nextRandomState(): State

  def possibleNextStates(): List[(State, Double)]
}

object State {
  case object Facebook extends State {
    val random = new Random()
    val reward = -1

    def nextRandomState() =
      if (State.Facebook.random.nextDouble() <= 0.9)
        this
      else
        State.Class1

    def possibleNextStates(): List[(State, Double)] =
      List((this, 0.9), (State.Class1, 0.1))

  }

  case object Class1 extends State {
    val random = new Random()
    val reward = -2

    def nextRandomState() =
      if (State.Class1.random.nextDouble() <= 0.5)
        State.Facebook
      else
        State.Class2

    def possibleNextStates() =
      List((State.Facebook, 0.5), (State.Class2, 0.5))
  }

  case object Class2 extends State {
    val random = new Random()
    val reward = -2

    def nextRandomState() =
      if (State.Class2.random.nextDouble() <= 0.8)
        State.Class3
      else
        State.Sleep

    def possibleNextStates() =
      List((State.Class3, 0.8), (State.Sleep, 0.2))
  }

  case object Class3 extends State {
    val random = new Random()
    val reward = -2

    def nextRandomState() =
      if (State.Class3.random.nextDouble() <= 0.6)
        State.Pass
      else
        State.Pub

    def possibleNextStates() =
      List((State.Pass, 0.6), (State.Pub, 0.4))
  }

  case object Pub extends State {
    val random = new Random()
    val reward = -1

    override def nextRandomState(): State = {
      val probability = State.Pub.random.nextDouble()
      if (probability <= 0.4)
        State.Class3
      else if (probability <= 0.8)
        State.Class2
      else
        State.Class1
    }

    def possibleNextStates() =
      List((State.Class1, 0.2), (State.Class2, 0.4), (State.Class3, 0.4))
  }

  case object Pass extends State {
    val reward = 10

    def nextRandomState() =
      State.Sleep

    def possibleNextStates() =
      List((State.Sleep, 1.0))
  }

  case object Sleep extends State {
    val reward = 0

    def nextRandomState() =
      State.Sleep

    def possibleNextStates() =
      List.empty
  }
}

object Agent {
  def calculateValue(sample: Iterable[State], discountFactor: Option[Double]): Double =
    sample.zipWithIndex.foldLeft(0.0) {
      case (totalReward, (state, position)) =>
        val discountRate =
          if (position == 0)
            1
          else
            discountFactor match {
              case Some(discountFactor) =>
                Math.pow(discountFactor, position)

              case None =>
                1
            }

        totalReward + (state.reward * discountRate)
    }
}
