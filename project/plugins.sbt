val SbtLucumaVersion = "0.17-9367033-SNAPSHOT"

// Hosts the sbt 2 build of sbt-lucuma, and the sbt-typelevel snapshot it depends on.
resolvers += "gemini-hlsw".at(
  "https://raw.githubusercontent.com/gemini-hlsw/maven-repo/master/releases"
)

addSbtPlugin("com.timushev.sbt"    % "sbt-updates"       % "0.7.0")
addSbtPlugin("edu.gemini"          % "sbt-lucuma-lib"    % SbtLucumaVersion)
addSbtPlugin("edu.gemini"          % "sbt-lucuma-docker" % SbtLucumaVersion)
addSbtPlugin("com.github.reibitto" % "sbt-test-shards"   % "0.3.0")
addSbtPlugin("com.eed3si9n"        % "sbt-buildinfo"     % "0.13.2")
addSbtPlugin("pl.project13.scala"  % "sbt-jmh"           % "0.4.8")

// OtelCheck.scala reads a pom; sbt 1 got scala-xml for free from Scala 2.12.
libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "2.4.0"
