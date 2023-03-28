ThisBuild / scalaVersion := "3.2.2"

lazy val root = (project in file("."))
  .aggregate(
    plain,
    types,
    types2,
    meta,
  )

lazy val plain = (project in file("plain"))
lazy val types = (project in file("types"))
lazy val types2 = (project in file("types2"))
lazy val meta = (project in file("meta"))
  .dependsOn(plain)
