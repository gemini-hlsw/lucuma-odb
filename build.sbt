val catsTimeVersion            = "0.5.1"
val circeVersion               = "0.14.3"
val cirisVersion               = "3.1.0"
val clueVersion                = "0.25.0"
val declineVersion             = "2.4.1"
val disciplineMunitVersion     = "1.0.9"
val flywayVersion              = "9.15.1"
val grackleVersion             = "0.10.3"
val http4sBlazeVersion         = "0.23.13"
val http4sEmberVersion         = "0.23.18"
val http4sJdkHttpClientVersion = "0.8.0"
val jwtVersion                 = "5.0.0"
val logbackVersion             = "1.4.5"
val log4catsVersion            = "2.5.0"
val lucumaCoreVersion          = "0.67.0"
val lucumaGraphQLRoutesVersion = "0.5.10"
val munitVersion               = "0.7.29"
val munitCatsEffectVersion     = "1.0.7"
val munitDisciplineVersion     = "1.0.9"
val natchezHttp4sVersion       = "0.5.0"
val natchezVersion             = "0.3.1"
val postgresVersion            = "42.5.4"
val skunkVersion               = "0.5.1"
val lucumaSsoVersion           = "0.5.2"
val lucumaItcVersion           = "0.6.0"
val testcontainersScalaVersion = "0.40.12"
val paigesVersion              = "0.4.2"

enablePlugins(NoPublishPlugin)

ThisBuild / Test / fork := false
ThisBuild / Test / parallelExecution := false
ThisBuild / scalaVersion       := "3.2.2"
ThisBuild / crossScalaVersions := Seq("3.2.2")

lazy val schema = project
  .in(file("modules/schema"))
  .settings(
    name := "lucuma-odb-schema",
    libraryDependencies ++= Seq(
      "io.circe"       %% "circe-parser"                    % circeVersion,
      "io.circe"       %% "circe-literal"                   % circeVersion,
      "io.circe"       %% "circe-refined"                   % circeVersion,
      "io.circe"       %% "circe-testing"                   % circeVersion               % Test,
      "edu.gemini"     %% "lucuma-core"                     % lucumaCoreVersion,
      "edu.gemini"     %% "lucuma-core-testkit"             % lucumaCoreVersion          % Test,
      "org.scalameta"  %% "munit"                           % munitVersion               % Test,
      "org.scalameta"  %% "munit-scalacheck"                % munitVersion               % Test,
      "org.typelevel"  %% "discipline-munit"                % munitDisciplineVersion     % Test,
    )
  )

lazy val sequence = project
  .in(file("modules/sequence"))
  .dependsOn(schema % "compile->test")
  .settings(
    name := "lucuma-odb-sequence"
  )

lazy val service = project
  .in(file("modules/service"))
  .dependsOn(sequence % "compile->test")
  .enablePlugins(JavaAppPackaging)
  .settings(
    name := "lucuma-odb-service",
    libraryDependencies ++= Seq(
      "ch.qos.logback" %  "logback-classic"                 % logbackVersion,
      "com.monovore"   %% "decline-effect"                  % declineVersion,
      "com.monovore"   %% "decline"                         % declineVersion,
      "edu.gemini"     %% "gsp-graphql-skunk"               % grackleVersion,
      "edu.gemini"     %% "lucuma-graphql-routes-grackle"   % lucumaGraphQLRoutesVersion,
      "edu.gemini"     %% "lucuma-itc-client"               % lucumaItcVersion,
      "edu.gemini"     %% "lucuma-sso-backend-client"       % lucumaSsoVersion,
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
      "org.scalameta"  %% "munit"                           % munitVersion               % Test,
      "org.scalameta"  %% "munit-scalacheck"                % munitVersion               % Test,
      "org.typelevel"  %% "cats-time"                       % catsTimeVersion,
      "org.typelevel"  %% "log4cats-slf4j"                  % log4catsVersion,
      "org.typelevel"  %% "munit-cats-effect-3"             % munitCatsEffectVersion     % Test,
      "org.typelevel"  %% "paiges-core"                     % paigesVersion,
      "com.github.vertical-blank" % "sql-formatter" % "2.0.3",
    ),
    reStart / envVars += "PORT" -> "8082",
    reStartArgs       += "-skip-migration"
  )

