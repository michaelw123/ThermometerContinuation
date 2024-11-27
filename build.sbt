ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.4"

lazy val root = (project in file("."))
  .settings(
    name := "ThermometerContinuation"
  )

libraryDependencies += "com.github.rssh" %% "dotty-cps-async" % "0.9.5"