name := "scala-playground"

version := "0.1"

scalaVersion := "2.12.6"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.2.5-PNF-SNAPSHOT")

// At some point, this will be the default
scalacOptions += "-Ypartial-unification"

resolvers += Resolver.sonatypeRepo("releases")
resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.3")

libraryDependencies += "com.olegpy" %% "better-monadic-for" % "0.2.5-PNF-SNAPSHOT"
libraryDependencies += "org.typelevel" %% "cats-core" % "1.2.0" withSources()
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5"//  % "test"
// libraryDependencies += "io.monix" %% "monix" % "3.0.0-RC1" // use fetch dependencies
libraryDependencies += "com.47deg" %% "fetch" % "0.7.3" withSources()
libraryDependencies += "com.47deg" %% "fetch-debug" % "0.7.3" withSources()
libraryDependencies += "com.47deg" %% "fetch-monix" % "0.7.3" withSources() // will pull in appropriate monix

scalacOptions ++= {
  Seq("-Xlog-implicits" ,"-Xlog-implicit-conversions") // ensures recompile
}
