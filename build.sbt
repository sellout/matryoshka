lazy val root = Project("root", file(".")).settings(
  scalaVersion := "2.11.8",
  resolvers ++= Seq(
    Resolver.bintrayRepo("tek", "maven"),
    Resolver.sonatypeRepo("releases")),
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  addCompilerPlugin("tryp"           %% "splain"   % "0.1.14"),
  libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.10.0")
