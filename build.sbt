name := "scala-playground"

version := "0.1"

scalaVersion := "2.12.6"

//addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.2.5-PNF-SNAPSHOT")

// At some point, this will be the default

resolvers += Resolver.sonatypeRepo("releases")
resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

//addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.3")
//addSbtPlugin("com.etsy" % "sbt-compile-quick-plugin" % "1.4.0")

//libraryDependencies += "com.olegpy" %% "better-monadic-for" % "0.2.5-PNF-SNAPSHOT"
 // will pull in appropriate monix

scalacOptions ++= {  Seq(/*"-Xlog-implicits" ,"-Xlog-implicit-conversions", */"-Ywarn-macros:both") }// ensures recompile}

lazy val core = (project in file ("core")).dependsOn(applicator).settings (Seq(
  scalacOptions += "-Ypartial-unification",
  //libraryDependencies += "org.typelevel" %% "cats-core" % "1.2.0" withSources(),
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5",//  % "test"
  // libraryDependencies += "io.monix" %% "monix" % "3.0.0-RC1" // use fetch dependencies instead
  libraryDependencies += "com.47deg" %% "fetch" % "1.0.0-RC1" withSources(),
  libraryDependencies += "io.monix" %% "monix" % "3.0.0-8084549" withSources(),
  //libraryDependencies += "com.47deg" %% "fetch-debug" % "1.0.0-RC1" withSources(),
  //libraryDependencies += "com.47deg" %% "fetch-monix" % "1.0.0-RC1" withSources(),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7"),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.2.4")
))

lazy val applicator = (project in file ("applicator")).settings (
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7"),
  libraryDependencies += "com.47deg" %% "fetch" % "1.0.0-RC1" withSources(),
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value withSources(),
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value withSources()
)
