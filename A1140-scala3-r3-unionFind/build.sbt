
scalaVersion := "3.3.3"

lazy val root = (project in file("."))
  .settings(
    name := "A1140-scala3-r3-unionFind",
    version := "1.0",
    Test / parallelExecution := false,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % Test,
  )
