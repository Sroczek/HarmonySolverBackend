resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots")
)
addSbtPlugin("org.openapitools" % "sbt-openapi-generator" % "5.0.0")
addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.8.0")