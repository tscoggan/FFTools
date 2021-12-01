ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.7"

lazy val root = (project in file("."))
  .settings(
    name := "FFTools",
    idePackagePrefix := Some("fftools")
  )

libraryDependencies += "com.typesafe" % "config" % "1.4.1"
