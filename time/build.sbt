name := "time"

organization := "time"

version := "0.1"

scalaVersion := "2.11.4"

resolvers ++= Seq(
  "Apache repo" at "https://repository.apache.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "joda-time" % "joda-time" % "2.8.2",
  "org.joda" % "joda-convert" % "1.2",
  "com.github.scopt" %% "scopt" % "3.3.0",
  "org.slf4j" % "slf4j-log4j12" % "1.7.12",
  "junit" % "junit" % "4.10",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test"
)
