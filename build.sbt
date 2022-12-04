val catsTimeVersion            = "0.5.1"
val circeVersion               = "0.14.3"
val cirisVersion               = "3.0.0"
val clueVersion                = "0.23.1"
val declineVersion             = "2.4.0"
val disciplineMunitVersion     = "1.0.9"
val flywayVersion              = "9.8.3"
val grackleVersion             = "0.8.2"
val http4sBlazeVersion         = "0.23.12"
val http4sEmberVersion         = "0.23.16"
val http4sJdkHttpClientVersion = "0.8.0"
val jwtVersion                 = "5.0.0"
val logbackVersion             = "1.4.5"
val log4catsVersion            = "2.5.0"
val lucumaCoreVersion          = "0.58.0"
val lucumaGraphQLRoutesVersion = "0.5.5"
val munitVersion               = "0.7.29"
val munitCatsEffectVersion     = "1.0.7"
val munitDisciplineVersion     = "1.0.9"
val natchezHttp4sVersion       = "0.3.2"
val natchezVersion             = "0.1.6"
val postgresVersion            = "42.5.1"
val skunkVersion               = "0.3.2"
val lucumaSsoVersion           = "0.4.3"
val testcontainersScalaVersion = "0.40.12"

enablePlugins(NoPublishPlugin)

ThisBuild / Test / fork := false
ThisBuild / Test / parallelExecution := false
ThisBuild / scalaVersion       := "3.2.1"
ThisBuild / crossScalaVersions := Seq("3.2.1")

lazy val service = project
  .in(file("modules/service"))
  .enablePlugins(JavaAppPackaging)
  .settings(
    name := "lucuma-odb-service",
    libraryDependencies ++= Seq(
      "ch.qos.logback" %  "logback-classic"                 % logbackVersion,
      "com.monovore"   %% "decline-effect"                  % declineVersion,
      "com.monovore"   %% "decline"                         % declineVersion,
      "edu.gemini"     %% "gsp-graphql-skunk"               % grackleVersion,
      "edu.gemini"     %% "lucuma-graphql-routes-grackle"   % lucumaGraphQLRoutesVersion,
      "edu.gemini"     %% "lucuma-sso-backend-client"       % lucumaSsoVersion,
      "edu.gemini"     %% "lucuma-core-testkit"             % lucumaCoreVersion          % Test,
      "io.circe"       %% "circe-parser"                    % circeVersion,
      "io.circe"       %% "circe-literal"                   % circeVersion,
      "io.circe"       %% "circe-refined"                   % circeVersion,
      "is.cir"         %% "ciris"                           % cirisVersion,
      "org.flywaydb"   %  "flyway-core"                     % flywayVersion,
      "org.http4s"     %% "http4s-jdk-http-client"          % http4sJdkHttpClientVersion,
      "org.http4s"     %% "http4s-blaze-server"             % http4sBlazeVersion,
      "org.http4s"     %% "http4s-ember-client"             % http4sEmberVersion,
      "org.postgresql" %  "postgresql"                      % postgresVersion,
      "org.tpolecat"   %% "natchez-honeycomb"               % natchezVersion,
      "org.tpolecat"   %% "natchez-http4s"                  % natchezHttp4sVersion,
      "org.tpolecat"   %% "natchez-log"                     % natchezVersion,
      "org.tpolecat"   %% "skunk-core"                      % skunkVersion,
      "com.dimafeng"   %% "testcontainers-scala-munit"      % testcontainersScalaVersion % Test,
      "com.dimafeng"   %% "testcontainers-scala-postgresql" % testcontainersScalaVersion % Test,
      "edu.gemini"     %% "clue-http4s"                     % clueVersion                % Test,
      "io.circe"       %% "circe-literal"                   % circeVersion               % Test,
      "org.scalameta"  %% "munit"                           % munitVersion               % Test,
      "org.scalameta"  %% "munit-scalacheck"                % munitVersion               % Test,
      "org.typelevel"  %% "cats-time"                       % catsTimeVersion,
      "org.typelevel"  %% "log4cats-slf4j"                  % log4catsVersion,
      "org.typelevel"  %% "munit-cats-effect-3"             % munitCatsEffectVersion     % Test,
      "org.typelevel"  %% "discipline-munit"                % munitDisciplineVersion     % Test,
      "com.github.vertical-blank" % "sql-formatter" % "2.0.3",
    ),
    reStart / envVars += "PORT" -> "8082",
    reStartArgs       += "-skip-migration"
  )

