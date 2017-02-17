import sbt.Keys._

lazy val root = project.in(file(".")).
  aggregate(validationJS, validationJVM)

lazy val validation = crossProject.in(file(".")).
  settings(
    scalaVersion := "2.12.0",
    crossScalaVersions := Seq("2.11.0", "2.12.0"),
    scalacOptions ++= Seq("-deprecation", "-feature"),
    organization := "io.underscore",
    name := "validation",
    version := "0.0.3",
    publishTo := Some(Resolver.file("file", new File("../maven-repo"))),
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5",
      "org.specs2" %% "specs2-core" % "3.8.8" % "test"
    ),
    scalacOptions in Test ++= Seq("-Yrangepos")
  )
  .jvmSettings(
    // Add JVM-specific settings here
  )
  .jsSettings(
    // Add JS-specific settings here
  )

lazy val validationJVM = validation.jvm
lazy val validationJS = validation.js

scalaVersion := "2.12.0"

crossScalaVersions := Seq("2.11.0")
