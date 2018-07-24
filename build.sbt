import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}

scalaVersion in ThisBuild := "2.12.6"

crossScalaVersions in ThisBuild := Seq("2.11.12", "2.12.6")

lazy val root = project.in(file("."))
  .aggregate(validationJS, validationJVM)
  .settings(
    publish := {},
    publishLocal := {}
  )

lazy val validation = crossProject(JSPlatform, JVMPlatform).in(file(".")).
  settings(
    scalacOptions ++= Seq("-deprecation", "-feature"),
    organization := "io.underscore",
    name := "validation",
    version := "0.0.4",
    publishTo := Some(Resolver.file("file", new File("../maven-repo"))),
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.typelevel" %% "cats-core" % "1.1.0",
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
