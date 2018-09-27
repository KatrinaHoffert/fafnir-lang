scalaVersion := "2.12.6"

name := "fafnir-lang"
organization := "io.github.katrinahoffert"
version := "0.1"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1" ,
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)
