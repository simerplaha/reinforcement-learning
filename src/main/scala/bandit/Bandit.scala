package bandit

import breeze.linalg._
import breeze.plot._

import scala.util.Random

/**
 * Chapter 2 - Multi arm bandit - Incremental Implementation
 */
object Environment {
  val random = new Random(new java.util.Random())

  //pull level and return the resulting reward.
  def pullLevel(lever: Int)(implicit environment: Environment): Int =
    if (random.nextDouble() < environment.probabilities(lever))
      1
    else
      0
}

case class Environment(probabilities: DenseVector[Double])

object Agent {

  val random = new Random(new java.util.Random())

  def optimalAction(agent: Agent): Int =
    if (random.nextDouble() < agent.epsilon)
      random.nextInt(agent.actionsCount.size) //explore
    else
      argmax(agent.rewards) //Greedy/Exploit

  def postAction(action: Int, reward: Int)(implicit agent: Agent): Unit = {
    agent.actionsCount(action) += 1 //store the number of times action is triggered
    //Chapter 2.4 - Multi-armed-bandit - Incremental Implementation formula.
    agent.rewards(action) += (reward - agent.rewards(action)) / agent.actionsCount(action)
  }
}

case class Agent(epsilon: Double,
                 actionsCount: DenseVector[Int],
                 rewards: DenseVector[Double])

object Bandit extends App {

  //rewards probability of each lever. The last level has the most reward and is expect in the plot.
  val rewardsPerLever = DenseVector(0.5, 0.10, 0.20, 0.25, 0.30, 0.50, 0.60, 0.65, 0.80, 0.90)
  val attempts = 10000 //max attempts/steps
  val epsilon = 0.1 //learning rate

  implicit val environment = Environment(rewardsPerLever)

  implicit val agent =
    Agent(
      epsilon = epsilon,
      actionsCount = DenseVector.zeros(rewardsPerLever.size),
      rewards = DenseVector.zeros(rewardsPerLever.size)
    )

  //run steps
  (1 to attempts) foreach {
    _ =>
      val action = Agent.optimalAction(agent)
      val reward = Environment.pullLevel(action)
      Agent.postAction(action, reward)
  }

  //plot
  val f = Figure()
  val p = f.subplot(0)
  val x = DenseVector(0 until rewardsPerLever.size: _*)
  p += plot(x, agent.actionsCount)
  p.xlabel = "Lever"
  p.ylabel = "Pull count"
  f.refresh()
}
