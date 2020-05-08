package tictactoe

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import util.Matrix

import scala.util.Random

class MatrixSpec extends AnyWordSpec with Matchers {

  "Matrix" should {
    "be empty on start" in {
      val matrix = Matrix.create[Option[Int]](2, 2, None)
      matrix.exists(_.isDefined) shouldBe false
      matrix.toSeq shouldBe Seq(Seq(None, None), Seq(None, None))
    }

    "update" in {
      val matrix = Matrix.create[Option[Int]](2, 2, None)

      matrix.updateCopy(0, 0, Some(10)).toSeq shouldBe Seq(Seq(Some(10), None), Seq(None, None))
      matrix.updateCopy(0, 1, Some(10)).toSeq shouldBe Seq(Seq(None, Some(10)), Seq(None, None))
      matrix.updateCopy(1, 0, Some(10)).toSeq shouldBe Seq(Seq(None, None), Seq(Some(10), None))
      matrix.updateCopy(1, 1, Some(10)).toSeq shouldBe Seq(Seq(None, None), Seq(None, Some(10)))
    }

    "exists" in {
      val matrix = Matrix.create[Option[Int]](2, 2, None)
      matrix.exists(_.isEmpty) shouldBe true

      (1 to 100) foreach {
        _ =>
          val updatedMatrix = matrix.updateCopy(Random.nextInt(2), Random.nextInt(2), Some(10))
          updatedMatrix.exists(_.contains(10)) shouldBe true
      }
    }
  }
}
