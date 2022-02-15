val circeVersion               = "0.14.1"
val cirisVersion               = "2.0.1"
val clueVersion                = "0.20.2"
val declineVersion             = "2.1.0"
val disciplineMunitVersion     = "1.0.9"
val flywayVersion              = "7.11.4"
val grackleVersion             = "0.1.16+17-6f247fe9+20220214-1513-SNAPSHOT"
val http4sVersion              = "0.23.6"
val jwtVersion                 = "5.0.0"
val log4catsVersion            = "2.1.1"
val lucumaCoreVersion          = "0.14.3"
val lucumaGraphQLRoutesVersion = "0.2.0"
val munitVersion               = "0.7.29"
val munitCatsEffectVersion     = "1.0.7"
val natcchezHttp4sVersion      = "0.2.0"
val natchezVersion             = "0.1.5"
val postgresVersion            = "42.3.1"
val skunkVersion               = "0.3.0"
val slf4jVersion               = "1.7.32"
val lucumaSsoVersion           = "0.0.13"
val testcontainersScalaVersion = "0.40.0"

inThisBuild(Seq(
  homepage := Some(url("https://github.com/gemini-hlsw/lucuma-sso")),
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full),
  testFrameworks += new TestFramework("munit.Framework"),
  Test / fork := true,
) ++ lucumaPublishSettings)

publish / skip := true

lazy val service = project
  .in(file("modules/service"))
  .enablePlugins(JavaAppPackaging)
  .settings(
    publish / skip := true,
    name := "lucuma-odb-service",
    scalacOptions --= Seq("-Vtype-diffs"),
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
      "org.http4s"     %% "http4s-blaze-server"             % http4sVersion,
      "org.http4s"     %% "http4s-ember-client"             % http4sVersion,
      "org.postgresql" %  "postgresql"                      % postgresVersion,
      "org.slf4j"      %  "slf4j-simple"                    % slf4jVersion,
      "org.tpolecat"   %% "natchez-honeycomb"               % natchezVersion,
      "org.tpolecat"   %% "natchez-http4s"                  % natcchezHttp4sVersion,
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

