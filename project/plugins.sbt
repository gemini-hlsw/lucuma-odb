val SbtLucumaVersion = "0.14.8"

addSbtPlugin("io.spray"            % "sbt-revolver"        % "0.10.0")
addSbtPlugin("com.timushev.sbt"    % "sbt-updates"         % "0.6.4")
addSbtPlugin("edu.gemini"          % "sbt-lucuma-lib"      % SbtLucumaVersion)
addSbtPlugin("edu.gemini"          % "sbt-lucuma-docker"   % SbtLucumaVersion)
addSbtPlugin("com.github.reibitto" % "sbt-test-shards"     % "0.2.0")
addSbtPlugin("com.eed3si9n"        % "sbt-buildinfo"       % "0.13.1")
addSbtPlugin("pl.project13.scala"  % "sbt-jmh"             % "0.4.8")