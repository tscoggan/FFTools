ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.7"

lazy val root = (project in file("."))
  .settings(
    name := "FFTools",
    idePackagePrefix := Some("fftools")
  )

libraryDependencies ++= Seq(
  "com.typesafe" % "config" % "1.4.1",
  "org.json4s" %% "json4s-jackson" % "4.0.3",
  "joda-time" % "joda-time" % "2.10.13"
)