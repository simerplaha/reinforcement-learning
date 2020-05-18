name := "reinforcement-learning"

version := "0.1"

scalaVersion := "2.13.1"

val breezeVersion = "1.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.1" % "test"
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"

libraryDependencies ++= Seq(
  "org.scalanlp" %% "breeze" % breezeVersion,
  "org.scalanlp" %% "breeze-natives" % breezeVersion,
  "org.scalanlp" %% "breeze-viz" % breezeVersion
)


