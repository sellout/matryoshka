resolvers += Resolver.sonatypeRepo("releases")

addSbtPlugin("com.47deg"        % "sbt-microsites"  % "0.7.13")
addSbtPlugin("org.scala-js"     % "sbt-scalajs"     % "0.6.22")
addSbtPlugin("org.scoverage"    % "sbt-scoverage"   % "1.5.1")
addSbtPlugin("com.slamdata"     % "sbt-slamdata"    % "0.5.6")
addSbtPlugin("com.eed3si9n"     % "sbt-unidoc"      % "0.4.1")
addSbtPlugin("com.timushev.sbt" % "sbt-updates"     % "0.3.4")
addSbtPlugin("org.wartremover"  % "sbt-wartremover" % "2.2.1")
