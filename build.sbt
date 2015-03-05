resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

scalaVersion := "2.11.5"

//scalacOptions += "-Xlog-implicits"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.1.1",
  "com.chuusai" %% "shapeless" % "2.1.0",
  "com.github.mpilquist" %% "simulacrum" % "0.3.0"
)