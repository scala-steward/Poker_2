ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "Poker2",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "2.2.0",
      "org.typelevel" %% "cats-core" % "2.2.0",
      "com.github.daddykotex" %% "courier" % "2.0.0",
      "com.amazonaws" % "aws-java-sdk-s3" % "1.12.710"
    )
  )
