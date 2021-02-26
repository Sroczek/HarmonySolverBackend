// Project setup
val scalaV   = "2.13.4"
val projectV = "1.0"

lazy val settings = Seq(
  version := projectV,
  scalaVersion := scalaV
)

lazy val testDependencies = Seq(
  "org.scalatest" %% "scalatest" % "3.0.8" % Test
)

/** projects */
lazy val rootProject = project
  .in(file("."))
  .settings(
    name := "HarmonySolverBackend",
    version := projectV
  )
  .aggregate(
    model,
    harmonics_parser
  )

lazy val model = project
  .settings(
    name := "model",
    settings,
    libraryDependencies ++= testDependencies
  )

lazy val harmonics_parser = project
  .settings(
    name := "harmonics_parser",
    settings,
    libraryDependencies ++= testDependencies ++ Seq("org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2")
  )
  .dependsOn(model)