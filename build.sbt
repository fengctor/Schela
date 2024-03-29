name := "Schela"

version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.29"

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

enablePlugins(PackPlugin)
packMain := Map("schela" -> "schela.Main")
packJvmOpts := Map("schela" -> Seq("-Xss512M", "-Xms2G", "-Xmx4G"))
