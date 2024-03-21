import Dependencies._

ThisBuild / scalaVersion     := "2.12.11"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "linkparser"
// ThisBuild / organizationName := "acme"

lazy val dependencies = Seq(
    "org.scalatest" %% "scalatest" % "3.1.2" % Test
  , "org.typelevel" %% "cats-core" % "2.1.1"
  , "org.scala-graph" %% "graph-core" % "1.13.1"
)

lazy val root = (project in file("."))
  .settings(
      name := "link-parser",
      javacOptions ++= Seq(
        "-Xlint",
        "-encoding", "UTF-8"
        ),
      scalacOptions ++= Seq(
        "-Xlint",
        "-Ywarn-dead-code",
        "-Ywarn-numeric-widen",
        "-Ywarn-unused",
        "-Ywarn-unused-import",
        "-unchecked",
        "-deprecation",
        "-feature",
        "-encoding", "UTF-8"
        ),
      cancelable := true,
      exportJars := true,
      libraryDependencies ++= dependencies
  )


