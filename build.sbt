import pl.project13.scala.sbt.SbtJmh._

lazy val commonSettings = Seq(
  scalaVersion := "2.11.8",
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-unchecked",
    "-Xfatal-warnings",
    "-Xfuture",
    "-Xlint",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard"
  ),
  initialCommands := """
    import purefun._
  """
) ++ scalariformSettings

lazy val purefun = project.in(file("."))
  .aggregate(bench, core)
  .dependsOn(bench, core)
  .settings(commonSettings)

lazy val bench = project
  .dependsOn(core)
  .settings(commonSettings)
  .settings(jmhSettings)

lazy val core = project
  .settings(commonSettings)
  .settings(
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.2" % "test"
  )
