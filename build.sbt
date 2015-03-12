name := "purefun"

scalaVersion := "2.11.6"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"

initialCommands := """
  import purefun._
"""

scalariformSettings
