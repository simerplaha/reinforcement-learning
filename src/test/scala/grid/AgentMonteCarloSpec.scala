package grid

import grid.AgentMonteCarlo.{Epsilon, StateAction}
import grid.Grid.{Action, Position}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Random

class AgentMonteCarloSpec extends AnyWordSpec with Matchers {

  implicit val grid =
    Grid.World(
      rows = 11,
      cols = 11,
      terminals = Seq((10, 10)),
      cellReward = 0,
      terminateReward = 1
    )

  "exploreOrExploit" should {
    "run all episode action" in {
      implicit val epsilon = Epsilon(0.0, new Random())

      val episode: List[Action[Double]] =
        List(Action.Up(0), Action.Right(0), Action.Down(0), Action.Right(0))

      val start = Grid.Position(2, 0)

      val actionValues: List[AgentMonteCarlo.StateAction] =
        AgentMonteCarlo.exploreOrExploit(
          position = start,
          resultStateActionValues = List.empty,
          episode = episode
        )

      val expected =
        List(
          StateAction(Position(2, 0), Action.Up(grid.cellReward)),
          StateAction(Position(1, 0), Action.Right(grid.cellReward)),
          StateAction(Position(1, 1), Action.Down(grid.cellReward)),
          StateAction(Position(2, 1), Action.Right(grid.terminateReward))
        )

      actionValues shouldBe expected
    }
  }

  "distributeRewards" should {

    "distribute" in {

      val episode: List[StateAction] =
        List(
          StateAction(Position(0, 0), Action.Up(grid.cellReward)),
          StateAction(Position(1, 0), Action.Right(grid.cellReward)),
          StateAction(Position(2, 0), Action.Down(grid.cellReward)),
          StateAction(Position(3, 0), Action.Right(grid.cellReward)),
          StateAction(Position(4, 0), Action.Up(grid.cellReward)),
          StateAction(Position(4, 0), Action.Right(grid.cellReward)),
          StateAction(Position(2, 0), Action.Down(grid.cellReward)),
          StateAction(Position(3, 0), Action.Right(grid.terminateReward))
        )

      val reward =
        AgentMonteCarlo.distributeRewards(
          gamma = 0.9,
          episode = episode
        )

      reward.size shouldBe episode.size

      //future rewards should be greater than previous reward.
      reward.foldLeft(0.0) {
        case (previousReward, stateAction) =>
          stateAction.action.value should be > previousReward
          stateAction.action.value
      }
    }
  }

  "stateValue" should {
    "calculate mean for duplicate states" in {
      val episode: List[StateAction] =
        List(
          StateAction(Position(0, 0), Action.Up(1)),
          StateAction(Position(0, 0), Action.Right(2)),
          StateAction(Position(0, 0), Action.Down(3)),
          StateAction(Position(0, 0), Action.Right(4)),
          StateAction(Position(1, 1), Action.Up(5)),
          StateAction(Position(1, 1), Action.Right(6)),
          StateAction(Position(1, 1), Action.Down(7)),
          StateAction(Position(1, 1), Action.Right(8))
        )

      val stateValues =
        AgentMonteCarlo.stateValue(stateActions = episode)
          .toList
          .sortBy(_._1.row)

      stateValues.size shouldBe 2

      stateValues.head._1 shouldBe Position(0, 0)
      stateValues.head._2 shouldBe ((1 + 2 + 3 + 4) / 4.0)

      stateValues.last._1 shouldBe Position(1, 1)
      stateValues.last._2 shouldBe ((5 + 6 + 7 + 8) / 4.0)
    }
  }

  "train" in {
    implicit val epsilon =
      Epsilon(
        exploreRate = 0.2,
        random = new Random()
      )

    val optimalPolicy = AgentMonteCarlo.train(maxPolicies = 1000)

    optimalPolicy.value should be > 0.0

    println(s"optimalPolicy value: ${optimalPolicy.value}")

    println
    println(optimalPolicy.stateDirectionMatrix)
    println
    println(optimalPolicy.stateValueMatrix)
  }

  "play test run" in {

    implicit val epsilon =
      Epsilon(
        exploreRate = 0.2,
        random = new Random()
      )

    val maxIterations = 100

    val policy = AgentMonteCarlo.Policy()

    val optimalPolicy =
      (1 to maxIterations).foldLeft(policy) {
        case (policy, _) =>
          val newPolicy =
            AgentMonteCarlo.run(
              state = Position(0, 0),
              policy = policy
            )

          println
          println(newPolicy.stateDirectionMatrix)
          println
          println(newPolicy.stateValueMatrix)

          if (newPolicy.value > policy.value) {
            println("\nNew policy is optimal")
            newPolicy
          } else {
            println("\nOld policy is optimal")
            policy
          }
      }

    println("\nOPTIMAL POLICY\n")
    println(optimalPolicy.stateDirectionMatrix)
    println
    println(optimalPolicy.stateValueMatrix)

  }
}
