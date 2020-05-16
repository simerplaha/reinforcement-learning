package util

import scala.reflect.ClassTag

object Vector {

  //creates a vector with one column.
  def apply[T: ClassTag](rowItems: T*): Matrix[T] =
    Matrix(rowItems.to(Array).map(item => Array(item)))

  //creates a vector with one column.
  def col[T: ClassTag](rowItems: T*): Matrix[T] =
    Vector(rowItems: _*)

  //creates a vector with one row.
  def row[T: ClassTag](colItems: T*): Matrix[T] =
    Matrix(Array(colItems: _*))

}
