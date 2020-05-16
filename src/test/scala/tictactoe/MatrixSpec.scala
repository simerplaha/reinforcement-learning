package tictactoe

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import util.Matrix

import scala.util.Random

class MatrixSpec extends AnyWordSpec with Matchers {

  "Matrix" should {
    "be empty on start" in {
      val matrix = Matrix.fill[Option[Int]](2, 2, None)
      matrix.exists(_.isDefined) shouldBe false
      matrix.toSeq shouldBe Seq(Seq(None, None), Seq(None, None))
    }

    "update" in {
      val matrix = Matrix.fill[Option[Int]](2, 2, None)

      matrix.updateCopy(0, 0, Some(10)).toSeq shouldBe Seq(Seq(Some(10), None), Seq(None, None))
      matrix.updateCopy(0, 1, Some(10)).toSeq shouldBe Seq(Seq(None, Some(10)), Seq(None, None))
      matrix.updateCopy(1, 0, Some(10)).toSeq shouldBe Seq(Seq(None, None), Seq(Some(10), None))
      matrix.updateCopy(1, 1, Some(10)).toSeq shouldBe Seq(Seq(None, None), Seq(None, Some(10)))
    }

    "exists" in {
      val matrix = Matrix.fill[Option[Int]](2, 2, None)
      matrix.exists(_.isEmpty) shouldBe true

      (1 to 100) foreach {
        _ =>
          val updatedMatrix = matrix.updateCopy(Random.nextInt(2), Random.nextInt(2), Some(10))
          updatedMatrix.exists(_.contains(10)) shouldBe true
      }
    }

    "multiple matrices" in {
      val result: Matrix[Int] = Matrix(Array(0, 3, 5), Array(5, 5, 2)) * Matrix(Array(3, 4), Array(3, -2), Array(4, -2))
      val expected: Matrix[Int] = Matrix(Array(29, -16), Array(38, 6))

      result.toSeq shouldBe expected.toSeq
    }

    "multiple vector" in {
      val m1 = Matrix(Array(1, 2, 3)).transpose
      val m2 = Matrix(Array(1, 2, 3))

      val result = m2 * m1
      result.toSeq should contain only List(14)
    }

    "add matrices" in {
      val result: Matrix[Int] = Matrix(Array(0, 3, 5), Array(5, 5, 2)) + Matrix(Array(3, 4, 1), Array(3, -2, 1))
      val expected: Matrix[Int] = Matrix(Array(3, 7, 6), Array(8, 3, 3))

      result.toSeq shouldBe expected.toSeq
    }

    "add vector" in {
      val m1 = Matrix(Array(1, 2, 3))
      val m2 = Matrix(Array(1, 2, 3))

      val result = m2 + m1
      result.toSeq.head shouldBe Seq(2, 4, 6)
    }
  }
}
