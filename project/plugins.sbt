val SbtLucumaVersion = "0.16.9"

addSbtPlugin("io.spray"            % "sbt-revolver"        % "0.10.0")
addSbtPlugin("com.timushev.sbt"    % "sbt-updates"         % "0.7.1")
addSbtPlugin("edu.gemini"          % "sbt-lucuma-lib"      % SbtLucumaVersion)
addSbtPlugin("edu.gemini"          % "sbt-lucuma-docker"   % SbtLucumaVersion)
addSbtPlugin("com.github.reibitto" % "sbt-test-shards"     % "0.3.0")
addSbtPlugin("com.eed3si9n"        % "sbt-buildinfo"       % "0.14.0")
addSbtPlugin("com.github.sbt"  % "sbt-jmh"             % "0.4.9")
