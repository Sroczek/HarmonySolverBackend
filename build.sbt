version := "1.0"
scalaVersion := "2.13.4"
parallelExecution in Test := false

libraryDependencies ++= Seq {
  "org.scalatest" %% "scalatest" % "3.0.8" % Test
}
