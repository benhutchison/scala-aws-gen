name := "scala-aws-gen"

scalaVersion := "2.12.1"

version := "0.1"

val circeVersion = "0.7.0"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

libraryDependencies += "io.circe" %% "circe-optics" % circeVersion

libraryDependencies += "org.atnos" %% "eff" % "4.0.0"

libraryDependencies += "org.typelevel" %% "cats" % "0.9.0"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")


scalacOptions += "-Ypartial-unification"
