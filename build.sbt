val circeVersion               = "0.14.2"
val cirisVersion               = "2.3.3"
val clueVersion                = "0.20.3"
val declineVersion             = "2.3.0"
val disciplineMunitVersion     = "1.0.9"
val flywayVersion              = "9.0.1"
val grackleVersion             = "0.3.0"
val http4sBlazeVersion         = "0.23.12"
val http4sEmberVersion         = "0.23.13"
val jwtVersion                 = "5.0.0"
val log4catsVersion            = "2.1.1"
val lucumaGraphQLRoutesVersion = "0.4.0"
val munitVersion               = "0.7.29"
val munitCatsEffectVersion     = "1.0.7"
val natchezHttp4sVersion       = "0.3.2"
val natchezVersion             = "0.1.6"
val postgresVersion            = "42.4.0"
val skunkVersion               = "0.3.1"
val slf4jVersion               = "1.7.36"
val lucumaSsoVersion           = "0.1.13"
val testcontainersScalaVersion = "0.40.9"

enablePlugins(NoPublishPlugin)

ThisBuild / Test / fork := true
ThisBuild / tlFatalWarnings := false // TODO!

lazy val service = project
  .in(file("modules/service"))
  .enablePlugins(JavaAppPackaging)
  .settings(
    name := "lucuma-odb-service",
    scalacOptions --= Seq("-Vtype-diffs"),
    scalacOptions ++= Seq("-Xcheckinit"),
    libraryDependencies ++= Seq(
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
      "org.slf4j"      %  "slf4j-simple"                    % slf4jVersion,
      "org.tpolecat"   %% "natchez-honeycomb"               % natchezVersion,
      "org.tpolecat"   %% "natchez-http4s"                  % natchezHttp4sVersion,
      "org.tpolecat"   %% "natchez-log"                     % natchezVersion,
      "org.tpolecat"   %% "skunk-core"                      % skunkVersion,
      "com.dimafeng"   %% "testcontainers-scala-munit"      % testcontainersScalaVersion % Test,
      "com.dimafeng"   %% "testcontainers-scala-postgresql" % testcontainersScalaVersion % Test,
      "edu.gemini"     %% "clue-http4s-jdk-client"          % clueVersion                % Test,
      "io.circe"       %% "circe-literal"                   % circeVersion               % Test,
      "org.scalameta"  %% "munit"                           % munitVersion               % Test,
      "org.typelevel"  %% "munit-cats-effect-3"             % munitCatsEffectVersion     % Test,
    ),
  )

