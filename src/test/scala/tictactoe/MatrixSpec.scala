package tictactoe

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Random

class MatrixSpec extends AnyWordSpec with Matchers {

  "Matrix" should {
    "be empty on start" in {
      val matrix = Matrix.create[Int](2, 2)
      matrix.exists(_.isDefined) shouldBe false
      matrix.elems shouldBe Seq(Seq(None, None), Seq(None, None))
    }

    "update" in {
      val matrix = Matrix.create[Int](2, 2)

      matrix.update(0, 0, 10).elems shouldBe Seq(Seq(Some(10), None), Seq(None, None))
      matrix.update(0, 1, 10).elems shouldBe Seq(Seq(None, Some(10)), Seq(None, None))
      matrix.update(1, 0, 10).elems shouldBe Seq(Seq(None, None), Seq(Some(10), None))
      matrix.update(1, 1, 10).elems shouldBe Seq(Seq(None, None), Seq(None, Some(10)))
    }

    "exists" in {
      val matrix = Matrix.create[Int](2, 2)
      matrix.exists(_.isEmpty) shouldBe true

      (1 to 100) foreach {
        _ =>
          val updatedMatrix = matrix.update(Random.nextInt(2), Random.nextInt(2), 10)
          updatedMatrix.exists(_.contains(10)) shouldBe true
      }
    }
  }
}
