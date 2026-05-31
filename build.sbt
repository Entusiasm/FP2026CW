ThisBuild / scalaVersion := "3.3.1"
ThisBuild / version := "1.0"

lazy val root = (project in file("."))
  .settings(
    name := "midi-analyzer",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "2.0.15",
      "dev.zio" %% "zio-streams" % "2.0.15"
    )
  )