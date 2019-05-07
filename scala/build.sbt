ThisBuild / scalaVersion := "2.12.8"
ThisBuild / organization := "de.dfki"

lazy val elevator = (project in file("."))
  .settings(
    name := "Elevator",
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-http" % "10.1.8",
      "com.typesafe.akka" %% "akka-stream" % "2.5.19",
      "io.circe" %% "circe-core" % "0.10.0",
      "io.circe" %% "circe-generic" % "0.10.0",
      "io.circe" %% "circe-parser" % "0.10.0",
      "io.circe" %% "circe-generic-extras" % "0.10.0")
)