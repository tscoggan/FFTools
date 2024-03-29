ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

lazy val root = (project in file("."))
  .settings(
    name := "FFTools",
  )

libraryDependencies ++= Seq(
  "com.typesafe" % "config" % "1.4.1",
  "org.json4s" %% "json4s-jackson" % "4.0.3",
  "joda-time" % "joda-time" % "2.10.13"
)