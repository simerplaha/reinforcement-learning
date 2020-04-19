package util

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object Util {

  /**
   * Performs the action [[f]] indefinitely until it returns no error.
   */
  @tailrec
  def tryUntilSuccess[T](f: => T): T =
    Try(f) match {
      case Failure(exception) =>
        println(exception.getMessage)
        tryUntilSuccess(f)

      case Success(value) =>
        value
    }
}
