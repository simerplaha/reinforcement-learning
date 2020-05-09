package lecture

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable.ListBuffer

class StudentSpec extends AnyWordSpec with Matchers {
  val optimalSample = Seq(State.Class1, State.Class2, State.Class3, State.Pass, State.Sleep)

  "optimal sample" in {

    //sample for Lecture 2: Markov Decision Process @ 27:09
    val sample2 = Seq(State.Class1, State.Facebook, State.Facebook, State.Class1, State.Class2, State.Sleep)
    val sample3 = Seq(State.Class1, State.Class2, State.Class3, State.Pub, State.Class2, State.Class3, State.Pass, State.Sleep)
    val sample4 = Seq(State.Facebook, State.Facebook, State.Facebook, State.Class1, State.Class2, State.Class3, State.Pub, State.Class2, State.Sleep)

    Agent.calculateValue(optimalSample, Some(0.5)) shouldBe -2.25

    Seq(optimalSample, sample2, sample3, sample4).foreach(sample => println("Value: " + Agent.calculateValue(sample, Some(0.5)) + " - Sample: " + sample))
  }

  "sampleReward" should {
    "assert reward is not duplicated for optimal" in {
      //sample for Lecture 2: Markov Decision Process @ 27:09. This checks that no other sample
      //returns reward greater than -2.25 optimal reward for the following particular sample.
      //best sample with -2.25 total reward.

      def generateSample: Iterable[State] = {
        val samples = ListBuffer(State.Class1: State)

        do
          samples += samples.last.nextRandomState()
        while (samples.last != State.Sleep)

        samples
      }

      val samples =
        (1 to 100).foldLeft(ListBuffer.empty[Iterable[State]]) {
          case (samples, _) =>
            samples += generateSample
        }

      samples.foreach {
        samples =>
          val sampleReward = Agent.calculateValue(samples, Some(0.5))

          //rewards cannot be greater than optiomal reward -2.25
          sampleReward should be <= -2.25

          if (sampleReward == -2.25)
            samples shouldBe optimalSample //ensure that there is no reward greater than the optimalSample

          println(samples + ": " + sampleReward)
      }
    }
  }
}
