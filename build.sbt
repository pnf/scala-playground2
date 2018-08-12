name := "scala-playground"

version := "0.1"

scalaVersion := "2.12.6"

scalacOptions += "-Ypartial-unification"

resolvers += Resolver.sonatypeRepo("releases")
resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.3")

libraryDependencies += "org.typelevel" %% "cats-core" % "1.2.0"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5"//  % "test"
libraryDependencies += "io.monix" %% "monix" % "3.0.0-RC1"
libraryDependencies += "com.47deg" %% "fetch" % "0.7.2"
libraryDependencies += "com.47deg" %% "fetch-monix" % "0.7.2"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")
