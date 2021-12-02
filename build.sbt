name := "Advent of Code 2021"

version := "0.1"

scalaVersion := "2.13.7"

val scalaTestVersion = "3.2.9"
val scalaticVersion = "3.2.9"

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % scalaticVersion,
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test
)