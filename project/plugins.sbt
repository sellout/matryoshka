resolvers += Resolver.sonatypeRepo("releases")

// FIXME: Not resolving cats-core_2.12-1.0.0-SNAPSHOT.
//addSbtPlugin("io.get-coursier"   % "sbt-coursier"   % "1.0.0-RC11")
addSbtPlugin("de.heikoseeberger" % "sbt-header"     % "1.6.0")
addSbtPlugin("com.fortysevendeg" % "sbt-microsites" % "0.3.3")
addSbtPlugin("org.scala-js"      % "sbt-scalajs"    % "0.6.14")
addSbtPlugin("org.scoverage"     % "sbt-scoverage"  % "1.5.0")
addSbtPlugin("com.typesafe.sbt"  % "sbt-site"       % "1.1.0")
addSbtPlugin("com.slamdata"      % "sbt-slamdata"   % "0.0.11")
addSbtPlugin("com.eed3si9n"      % "sbt-unidoc"     % "0.3.3")
