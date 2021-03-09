// Project setup
val scalaV   = "2.13.4"
val projectV = "1.0"

externalResolvers += Resolver.sonatypeRepo("snapshots")

lazy val settings = Seq(
  version := projectV,
  scalaVersion := scalaV
)

lazy val testDependencies = Seq(
  "org.scalatest" %% "scalatest" % "3.0.8" % Test
)

lazy val playDependencies = Seq(
  guice,
  ws,
  "org.webjars" % "swagger-ui" % "3.1.5",
  "javax.annotation" % "javax.annotation-api" % "1.3.2" % "compile",
  "org.scalatestplus.play" %% "scalatestplus-play" % "5.1.0" % Test
)

/** projects */
lazy val rootProject = project
  .in(file("."))
  .settings(
    name := "HarmonySolverBackend",
    version := projectV
  )
  .aggregate(
    rest
  )

lazy val rest = project
  .in(file("rest"))
  .enablePlugins(OpenApiGeneratorPlugin)
  .enablePlugins(PlayScala)
  .settings(
    openApiGeneratorName := "scala-play-server",
    openApiInputSpec := file("rest/src/main/resources/harmonySolverApi.yaml").getPath,
    openApiConfigFile := file("rest/src/main/resources/config.yaml").getPath,
    openApiOutputDir := file("rest").getPath,
    openApiValidateSpec := SettingDisabled,
    openApiGenerateModelTests := SettingEnabled,
    name := "rest",
    settings,
    libraryDependencies ++= testDependencies,
    libraryDependencies ++= playDependencies
  )
