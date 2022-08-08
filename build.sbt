val circeVersion               = "0.14.2"
val cirisVersion               = "2.3.3"
val clueVersion                = "0.20.3"
val declineVersion             = "2.3.0"
val disciplineMunitVersion     = "1.0.9"
val flywayVersion              = "9.1.2"
val grackleVersion             = "0.4.0"
val http4sBlazeVersion         = "0.23.12"
val http4sEmberVersion         = "0.23.14"
val jwtVersion                 = "5.0.0"
val logbackVersion             = "1.2.11"
val log4catsVersion            = "2.4.0"
val lucumaGraphQLRoutesVersion = "0.5.0"
val munitVersion               = "0.7.29"
val munitCatsEffectVersion     = "1.0.7"
val natchezHttp4sVersion       = "0.3.2"
val natchezVersion             = "0.1.6"
val postgresVersion            = "42.4.1"
val skunkVersion               = "0.3.1"
val lucumaSsoVersion           = "0.1.14"
val testcontainersScalaVersion = "0.40.10"

enablePlugins(NoPublishPlugin)

ThisBuild / Test / fork := true

lazy val service = project
  .in(file("modules/service"))
  .enablePlugins(JavaAppPackaging)
  .settings(
    name := "lucuma-odb-service",
    scalacOptions --= Seq("-Vtype-diffs"),
    scalacOptions ++= Seq("-Xcheckinit"),
    libraryDependencies ++= Seq(
      "ch.qos.logback" %  "logback-classic"                 % logbackVersion,
      "com.monovore"   %% "decline-effect"                  % declineVersion,
      "com.monovore"   %% "decline"                         % declineVersion,
      "edu.gemini"     %% "gsp-graphql-skunk"               % grackleVersion,
      "edu.gemini"     %% "lucuma-graphql-routes-grackle"   % lucumaGraphQLRoutesVersion,
      "edu.gemini"     %% "lucuma-sso-backend-client"       % lucumaSsoVersion,
      "io.circe"       %% "circe-parser"                    % circeVersion,
      "io.circe"       %% "circe-refined"                   % circeVersion,
      "is.cir"         %% "ciris"                           % cirisVersion,
      "org.flywaydb"   %  "flyway-core"                     % flywayVersion,
      "org.http4s"     %% "http4s-blaze-server"             % http4sBlazeVersion,
      "org.http4s"     %% "http4s-ember-client"             % http4sEmberVersion,
      "org.postgresql" %  "postgresql"                      % postgresVersion,
      "org.tpolecat"   %% "natchez-honeycomb"               % natchezVersion,
      "org.tpolecat"   %% "natchez-http4s"                  % natchezHttp4sVersion,
      "org.tpolecat"   %% "natchez-log"                     % natchezVersion,
      "org.tpolecat"   %% "skunk-core"                      % skunkVersion,
      "com.dimafeng"   %% "testcontainers-scala-munit"      % testcontainersScalaVersion % Test,
      "com.dimafeng"   %% "testcontainers-scala-postgresql" % testcontainersScalaVersion % Test,
      "edu.gemini"     %% "clue-http4s-jdk-client"          % clueVersion                % Test,
      "io.circe"       %% "circe-literal"                   % circeVersion               % Test,
      "org.scalameta"  %% "munit"                           % munitVersion               % Test,
      "org.typelevel"  %% "log4cats-slf4j"                  % log4catsVersion,
      "org.typelevel"  %% "munit-cats-effect-3"             % munitCatsEffectVersion     % Test,
      "com.github.vertical-blank" % "sql-formatter" % "2.0.3",
    ),
    reStart / envVars += "PORT" -> "8082",
    reStartArgs       += "-skip-migration"
  )

