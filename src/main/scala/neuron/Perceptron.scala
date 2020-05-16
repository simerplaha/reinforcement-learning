package neuron

import util.{Matrix, Vector}

import scala.util.Random

/**
 * Simple Perceptron/Neuron example that returns the first value from the input values
 * without explicit defining the rule.
 */
object Perceptron {

  //4x3 matrix
  val trainingInputs: Matrix[Double] =
    Matrix(
      Array(0, 0, 1),
      Array(1, 1, 1),
      Array(1, 0, 1),
      Array(0, 1, 1)
    )

  //4x1 matrix
  val trainingOutputs: Matrix[Double] =
    Vector(
      0,
      1,
      1,
      0
    )

  def sigmoid(matrix: Matrix[Double]): Matrix[Double] =
    matrix map {
      value =>
        1 / (1 + Math.exp(-value))
    }

  def sigmoidDerivative(matrix: Matrix[Double]): Matrix[Double] =
    matrix map {
      value =>
        value * (1 - value)
    }

  def train(iterations: Int = 100000): Matrix[Double] = {
    //some random weight which will eventually get adjusted.
    val initialWeight =
      Vector(
        Math.random(),
        Math.random(),
        Math.random()
      )

    println("\ninitialWeight: ")
    println(initialWeight)

    //returns the trained weight based on the expected output.
    (1 to iterations).foldLeft(initialWeight) {
      case (weights, _) =>
        val actualOutput = predictWeight(trainingInputs, weights)
        val derivative = sigmoidDerivative(actualOutput)
        val error = trainingOutputs - actualOutput
        val errorDerivative = error * derivative
        val adjustments = trainingInputs.transpose * errorDerivative
        weights + adjustments
    }
  }

  /**
   * Predict the output weight of the input matrix from the known weights.
   */
  def predictWeight(input: Matrix[Double], weights: Matrix[Double]): Matrix[Double] =
    sigmoid(input * weights)

  /**
   * Using the input weights predict the output.
   */
  def predictValue(input: Array[Double], weights: Matrix[Double]): Int =
    predictWeight(Matrix(input), weights)
      .column(0)
      .zip(input)
      .minBy(_._1) //sorted based on the weight and return the value with the highest weight.
      ._2
      .toInt

  /**
   * Trains the perceptron and asserts predictions.
   */
  def trainAndTest() = {
    //get the training weights.
    val weights = train()
    println("\nTrained Weights: ")
    println(weights)
    println

    def oneOrZero = if (Random.nextBoolean()) 0 else 1

    (1 to 10) foreach {
      testNumber =>
        val input = Array(oneOrZero, oneOrZero, oneOrZero)
        println(s"Test: $testNumber")
        println(Matrix(input))

        val result = predictValue(input.map(_.toDouble), weights)
        println(s"Result: $result\n")

        //assert that the prediction returns the first
        assert(result == input.head, result + " != " + input.head)
    }
  }

  def main(args: Array[String]): Unit =
    trainAndTest()
}
