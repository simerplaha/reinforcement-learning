package grid

import grid.Grid.{Action, Position}
import util.Matrix

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._
import scala.util.Random

/**
 * Monte Carlo Reinforcement Learning.
 *
 * This Agent run on [[Grid.World]] similar to [[AgentDynamicProgramming]].
 *
 * In [[AgentDynamicProgramming]] we had the knowledge of the full MDP (Model). But
 * in [[AgentMonteCarlo]] the agent learns by exploring the Grid without any prior knowledge.
 * The agent's goal is to find
 * - Optimal action per state
 * - Optimal policy from multiple policies
 */
object AgentMonteCarlo {

  /**
   * Explores or exploits an episode.
   *
   * @param position                starting position
   * @param resultStateActionValues outcome of this function
   * @param episode                 episode/actions to run
   * @param world                   grid world to run episodes on
   * @param epsilon                 rate of exploration
   * @return returns the new State and Action for the input episode.
   */
  @tailrec
  private[grid] def exploreOrExploit(position: Position,
                                     resultStateActionValues: List[StateAction],
                                     episode: List[Action[Double]])(implicit world: Grid.World,
                                                                    epsilon: Epsilon): List[StateAction] =
    episode match {
      case currentAction :: remainingActions =>
        val action = //pick an action. Either exploit or explore.
          if (epsilon.exploit())
            currentAction
          else
            Action.randomAction(0.0)

        //execute the action
        world.move(from = position, action = action) match {
          case Some(nextPosition) =>
            val nextReward = world.reward(nextPosition)
            val updatedAction = action.copyValue(nextReward) //update the executed action's value to the move's reward.
            //collect all StateAction values so that we track the actions performed in erach state.
            val stateActions = resultStateActionValues :+ StateAction(position, updatedAction)

            //if terminal is reach then exit. Should it continue exploring regardless?
            val exit = world.get(nextPosition) == Grid.Terminal

            if (exit)
              stateActions
            else
              exploreOrExploit(nextPosition, stateActions, remainingActions)

          case None =>
            exploreOrExploit(position, resultStateActionValues, remainingActions)
        }

      case Nil =>
        resultStateActionValues
    }

  /**
   * Since we are expecting rewards at the end of the episode we need to distribute the reward back
   * up to the root Action.
   *
   * Operates on reversed actions.
   */
  def distributeRewards(gamma: Double, episode: List[StateAction])(implicit world: Grid.World): List[StateAction] =
    episode
      .reverse
      .foldLeft((List.empty[StateAction], Option.empty[Action[Double]])) {
        case ((buffer, previousAction), stateAction) =>
          previousAction match {
            case Some(previousAction) =>
              val reward = stateAction.action.value + (gamma * previousAction.value)
              val updatedAction = stateAction.action.copyValue(reward)
              val updatedStateAction = stateAction.copy(action = updatedAction)
              val newBuffer = updatedStateAction +: buffer
              (newBuffer, Some(updatedAction))

            case None =>
              val newBuffer = stateAction +: buffer
              (newBuffer, Some(stateAction.action))
          }
      }._1

  /**
   * Runs the episode and returns the update StateAction pair.
   */
  def runEpisode(position: Position,
                 gamma: Double,
                 episode: List[Action[Double]])(implicit world: Grid.World,
                                                epsilon: Epsilon): List[StateAction] = {
    val exploredActions =
      exploreOrExploit(
        position = position,
        resultStateActionValues = List.empty,
        episode = episode
      )

    distributeRewards(
      gamma = gamma,
      episode = exploredActions
    )
  }

  /**
   * Calculates the value of each state.
   *
   * TODO - Incremental mean.
   */
  def stateValue(stateActions: Seq[StateAction]): Map[Position, Grid.Action[Double]] =
    stateActions
      .groupBy(_.position)
      .map {
        case (state, actions) =>
          val sum =
            actions.foldLeft(0.0) {
              case (sum, stateAction) =>
                sum + stateAction.action.value
            }

          val mean = sum / actions.size

          val optimalAction = actions.map(_.action).max.copyValue(mean)

          (state, optimalAction)
      }

  /**
   * Executes a [[Policy]] from the a [[Position]].
   */
  def run(state: Position,
          policy: Policy)(implicit world: Grid.World,
                          epsilon: Epsilon): Policy = {
    //run the episode and get the StateAction values
    val stateActions: List[StateAction] =
      runEpisode(
        position = state,
        gamma = policy.gamma,
        episode = policy.actionValue
      )

    //get the state values and optimal action for each state
    val newStateValues: Map[Position, Action[Double]] =
      stateValue(stateActions)

    //calculate policy value
    val policyStateValueSum =
      newStateValues.foldLeft(0.0) {
        case (sum, (_, optimalAction)) =>
          sum + optimalAction.value
      }

    val policyValue = policyStateValueSum / newStateValues.size

    val newActionValues: List[Action[Double]] =
      stateActions.map(_.action)

    //initial policy.
    Policy(
      value = policyValue,
      gamma = policy.gamma,
      stateValue = newStateValues,
      actionValue = newActionValues
    )
  }

  /**
   * Executes multiple policies concurrently and returns the optimal policy.
   */
  def train(gamma: Double = 0.9,
            position: Position = Position(0, 0),
            maxPolicies: Int)(implicit world: Grid.World,
                              epsilon: Epsilon): Policy = {
    val policies = //concurrently find an optimal policy.
      AgentMonteCarlo.Policy.many(gamma, maxPolicies).par map {
        policy =>
          run(position, policy)
      }

    policies.foldLeft(policies.head) {
      case (policy1, policy2) =>
        Policy.ordering.max(policy1, policy2)
    }
  }

  /**
   * Generates random actions.
   */
  def generateEpisode(implicit world: Grid.World): List[Action[Double]] = {
    val minimumActions = (world.rows * world.cols) * 100
    List.fill(minimumActions)(Action.randomAction(0.0))
  }

  object Policy {
    implicit val ordering = Ordering.by[Policy, Double](_.value)

    //initialises single policy
    def apply(gamma: Double = 0.9)(implicit world: Grid.World,
                                   epsilon: Epsilon): Policy =
      new Policy(
        value = 0.0,
        gamma = gamma,
        stateValue = Map.empty,
        actionValue = generateEpisode
      )

    //initialises multiple policy
    def many(gamma: Double = 0.9,
             count: Int)(implicit world: Grid.World): Seq[Policy] =
      (1 to count) map {
        _ =>
          new Policy(
            value = 0.0,
            gamma = gamma,
            stateValue = Map.empty,
            actionValue = generateEpisode
          )
      }
  }

  /**
   * Stores the Policy variables
   *
   * @param value       the policy's value. Uses to determine how good is this policy vs other policies.
   * @param gamma       the rate of reward distribution
   * @param stateValue  stores optional action for each state
   * @param actionValue current know optimal actions to perform for this Policy.
   */
  case class Policy(value: Double,
                    gamma: Double,
                    stateValue: Map[Position, Grid.Action[Double]],
                    actionValue: List[Grid.Action[Double]]) {

    /**
     * Converts the state value function to a value matrix for visualisation
     */
    def stateValueMatrix(implicit world: Grid.World): Matrix[Double] =
      Matrix.fill[Double](world.rows, world.cols) {
        case (row, col) =>
          stateValue.get(Position(row, col)) match {
            case Some(value) =>
              BigDecimal(value.value).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble

            case None =>
              0.0
          }
      }

    /**
     * Converts the state value function to a direction matrix for visualisation
     */
    def stateDirectionMatrix(implicit world: Grid.World): Matrix[String] =
      Matrix.fill[String](world.rows, world.cols) {
        case (row, col) =>
          val optimalAction = stateValue.get(Position(row, col))
          //terminals
          if (world.get(row, col) == Grid.Terminal) {
            //Terminal/End/Goal Grid's value is not computed.
            assert(optimalAction.isEmpty)
            "⌂"
          } else {
            optimalAction match {
              case Some(value) =>
                value.icon

              case None =>
                ""
            }
          }
      }
  }

  /**
   * Stores the optimal Action for each State.
   */
  case class StateAction(position: Position, action: Action[Double])

  object Epsilon {
    def apply(exploreRate: Double): Epsilon =
      new Epsilon(exploreRate, new Random())
  }

  /**
   * Epsilon logic.
   */
  case class Epsilon(exploreRate: Double, random: Random) {
    def exploit(): Boolean =
      random.nextDouble() > exploreRate

    def explore(): Boolean =
      !exploit
  }
}
