val scala3Version = "3.0.1"

inThisBuild(
  List(
    version := "1.0.0",
    scalaVersion := scala3Version,
    scalafmtOnCompile := true
  )
)

lazy val root = project
  .in(file("."))
  .aggregate(shared, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10)
  .settings(
    name := "Advent of Code 2020"
  )

lazy val shared = project
  .in(file("shared"))
  .settings(
    name := "Shared"
  )

lazy val d1 = project
  .in(file("d1"))
  .dependsOn(shared)
  .settings(
    name := "Day 1"
  )

lazy val d2 = project
  .in(file("d2"))
  .dependsOn(shared)
  .settings(
    name := "Day 2"
  )

lazy val d3 = project
  .in(file("d3"))
  .dependsOn(shared)
  .settings(
    name := "Day 3"
  )

lazy val d4 = project
  .in(file("d4"))
  .dependsOn(shared)
  .settings(
    name := "Day 4"
  )

lazy val d5 = project
  .in(file("d5"))
  .dependsOn(shared)
  .settings(
    name := "Day 5"
  )

lazy val d6 = project
  .in(file("d6"))
  .dependsOn(shared)
  .settings(
    name := "Day 6"
  )

lazy val d7 = project
  .in(file("d7"))
  .dependsOn(shared)
  .settings(
    name := "Day 7"
  )

lazy val d8 = project
  .in(file("d8"))
  .dependsOn(shared)
  .settings(
    name := "Day 8"
  )

lazy val d9 = project
  .in(file("d9"))
  .dependsOn(shared)
  .settings(
    name := "Day 9"
  )

lazy val d10 = project
  .in(file("d10"))
  .dependsOn(shared)
  .settings(
    name := "Day 10"
  )

addCommandAlias("cd", "project")
addCommandAlias("ls", "projects")
addCommandAlias("c", "compile")
addCommandAlias("rel", "reload")
addCommandAlias("r", "run")
