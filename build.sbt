

organization := "org.ergoplatform"
name := "autoleakus"
version := "0.1.0"
scalaVersion := "2.12.7"
resolvers ++= Seq("Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "SonaType" at "https://oss.sonatype.org/content/groups/public",
  "Typesafe maven releases" at "http://repo.typesafe.com/typesafe/maven-releases/",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/")
homepage := Some(url("http://ergoplatform.org/"))
licenses := Seq("CC0" -> url("https://creativecommons.org/publicdomain/zero/1.0/legalcode"))


libraryDependencies ++= Seq(
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  ("org.scorexfoundation" %% "sigma-state" % "0.10.0")
    .exclude("ch.qos.logback", "logback-classic"),

  "com.storm-enroute" %% "scalameter" % "0.8.+" % "test",
  "org.scalactic" %% "scalactic" % "3.0.+" % "test",
  "org.scalatest" %% "scalatest" % "3.0.+" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.+" % "test"
)
