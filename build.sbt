ThisBuild / version := "1.0.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.0"

lazy val ScalaTestVersion = "3.2.15"

lazy val root = (project in file("."))
  .settings(
    name := "scala-cookbok",
    idePackagePrefix := Some("com.mucciolo"),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % ScalaTestVersion % Test,
    ),
  )
