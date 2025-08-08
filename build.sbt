val awsJavaSdkVersion          = "1.12.787"
val catsParseVersion           = "1.1.0"
val catsTimeVersion            = "0.6.0"
val circeVersion               = "0.14.14"
val circeRefinedVersion        = "0.15.1"
val cirisVersion               = "3.9.0"
val clueVersion                = "0.46.0"
val declineVersion             = "2.5.0"
val disciplineMunitVersion     = "1.0.9"
val flywayVersion              = "9.20.0"
val fs2AwsVersion              = "6.2.0"
val fs2Version                 = "3.12.0"
val grackleVersion             = "0.24.0"
val http4sBlazeVersion         = "0.23.17"
val http4sEmberVersion         = "0.23.30"
val http4sJdkHttpClientVersion = "0.10.0"
val jwtVersion                 = "5.0.0"
val logbackVersion             = "1.5.18"
val log4catsVersion            = "2.7.1"
val lucumaItcVersion           = "0.42.4"
val lucumaCoreVersion          = "0.141.0"
val lucumaGraphQLRoutesVersion = "0.10.1"
val lucumaSsoVersion           = "0.9.5"
val munitVersion               = "0.7.29"  // check test output if you attempt to update this
val munitCatsEffectVersion     = "1.0.7"   // check test output if you attempt to update this
val munitDisciplineVersion     = "1.0.9"   // check test output if you attempt to update this
val natchezHttp4sVersion       = "0.6.1"
val natchezVersion             = "0.3.8"
val paigesVersion              = "0.4.4"
val postgresVersion            = "42.7.7"
val pprintVersion              = "0.9.3"
val skunkVersion               = "0.6.4"
val testcontainersScalaVersion = "0.40.14" // check test output if you attempt to update this

ThisBuild / tlBaseVersion      := "0.27"
ThisBuild / scalaVersion       := "3.7.1"
ThisBuild / crossScalaVersions := Seq("3.7.1")

ThisBuild / Test / fork              := false
ThisBuild / Test / parallelExecution := false

ThisBuild / githubWorkflowSbtCommand := "sbt -v -J-Xmx6g"
// ThisBuild / githubWorkflowBuildPreamble +=
//   WorkflowStep.Use(
//     UseRef.Public("gemini-hlsw", "migration-validator-action", "main"),
//     name = Some("Validate Migrations"),
//     params = Map("path" -> "modules/service/src/main/resources/db/migration/"),
//     cond = Some("github.event_name == 'pull_request'  && matrix.shard == '1'")
//   )

ThisBuild / githubWorkflowBuildPreamble +=
  WorkflowStep.Use(
    UseRef.Public("kamilkisiela", "graphql-inspector", "master"),
    name = Some("Validate GraphQL schema changes"),
    params =
      Map("schema"        -> "main:modules/schema/src/main/resources/lucuma/odb/graphql/OdbSchema.graphql",
          "approve-label" -> "expected-breaking-change"
      ),
    cond = Some("github.event_name == 'pull_request' && matrix.shard == '1'")
  )

val nTestJobShards = 8

ThisBuild / githubWorkflowBuildMatrixAdditions += (
  "shard" -> ((0 to (nTestJobShards - 1)).map(_.toString).toList)
)
ThisBuild / githubWorkflowBuild ~= (_.map(step =>
  if (step.name.contains("Test"))
    step.withEnv(
      Map("TEST_SHARD_COUNT" -> nTestJobShards.toString(), "TEST_SHARD" -> "${{ matrix.shard }}")
    )
  else step
))

lazy val schema =
  crossProject(JVMPlatform, JSPlatform)
    .crossType(CrossType.Pure)
    .in(file("modules/schema"))
    .settings(
      name := "lucuma-odb-schema",
      libraryDependencies ++= Seq(
        "io.circe"      %%% "circe-parser"               % circeVersion,
        "io.circe"      %%% "circe-literal"              % circeVersion,
        "io.circe"      %%% "circe-refined"              % circeRefinedVersion,
        "io.circe"      %%% "circe-testing"              % circeVersion           % Test,
        "edu.gemini"    %%% "lucuma-core"                % lucumaCoreVersion,
        "edu.gemini"    %%% "lucuma-core-testkit"        % lucumaCoreVersion      % Test,
        "edu.gemini"    %%% "lucuma-sso-frontend-client" % lucumaSsoVersion,
        "org.scalameta" %%% "munit"                      % munitVersion           % Test,
        "org.scalameta" %%% "munit-scalacheck"           % munitVersion           % Test,
        "org.typelevel" %%% "discipline-munit"           % munitDisciplineVersion % Test
      )
    )

lazy val binding = project
  .in(file("modules/binding"))
  .dependsOn(schema.jvm)
  .settings(
    name := "lucuma-odb-binding",
    tlVersionIntroduced := Map("3" -> "0.19.3"),
    libraryDependencies ++= Seq(
      "edu.gemini"    %% "lucuma-core"        % lucumaCoreVersion,
      "org.typelevel" %% "grackle-core"       % grackleVersion,
      "org.scalameta" %% "munit"              % munitVersion           % Test,
    )
  )

lazy val sequence = project
  .in(file("modules/sequence"))
  .dependsOn(schema.jvm)
  .enablePlugins(NoPublishPlugin)
  .settings(
    name := "lucuma-odb-sequence",
    libraryDependencies ++= Seq(
      "edu.gemini"    %% "lucuma-itc-client"  % lucumaItcVersion,
      "edu.gemini"    %% "lucuma-itc-testkit" % lucumaItcVersion       % Test,
      "org.scalameta" %% "munit"              % munitVersion           % Test,
      "org.scalameta" %% "munit-scalacheck"   % munitVersion           % Test,
      "org.typelevel" %% "discipline-munit"   % munitDisciplineVersion % Test
    )
  )

lazy val smartgcal = project
  .in(file("modules/smartgcal"))
  .enablePlugins(NoPublishPlugin)
  .settings(
    name := "lucuma-odb-smartgcal",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-parse"          % catsParseVersion,
      "co.fs2"        %% "fs2-core"            % fs2Version,
      "co.fs2"        %% "fs2-io"              % fs2Version,
      "edu.gemini"    %% "lucuma-core"         % lucumaCoreVersion,
      "edu.gemini"    %% "lucuma-core-testkit" % lucumaCoreVersion      % Test,
      "org.scalameta" %% "munit"               % munitVersion           % Test,
      "org.scalameta" %% "munit-scalacheck"    % munitVersion           % Test,
      "org.typelevel" %% "discipline-munit"    % munitDisciplineVersion % Test
    )
  )

lazy val service = project
  .in(file("modules/service"))
  .dependsOn(binding, phase0, sequence, smartgcal)
  .enablePlugins(NoPublishPlugin, JavaAppPackaging)
  .settings(
    name                        := "lucuma-odb-service",
    projectDependencyArtifacts  := (Compile / dependencyClasspathAsJars).value,
    libraryDependencies ++= Seq(
      "ch.qos.logback"            % "logback-classic"                    % logbackVersion,
      "com.monovore"             %% "decline-effect"                     % declineVersion,
      "com.monovore"             %% "decline"                            % declineVersion,
      "io.laserdisc"             %% "fs2-aws-s3"                         % fs2AwsVersion,
      "org.typelevel"            %% "grackle-skunk"                      % grackleVersion,
      "edu.gemini"               %% "lucuma-catalog"                     % lucumaCoreVersion,
      "edu.gemini"               %% "lucuma-ags"                         % lucumaCoreVersion,
      "edu.gemini"               %% "lucuma-graphql-routes"              % lucumaGraphQLRoutesVersion,
      "edu.gemini"               %% "lucuma-sso-backend-client"          % lucumaSsoVersion,
      "is.cir"                   %% "ciris"                              % cirisVersion,
      "is.cir"                   %% "ciris-refined"                      % cirisVersion,
      "org.flywaydb"              % "flyway-core"                        % flywayVersion,
      "org.http4s"               %% "http4s-blaze-server"                % http4sBlazeVersion,
      "org.http4s"               %% "http4s-ember-client"                % http4sEmberVersion,
      "org.postgresql"            % "postgresql"                         % postgresVersion,
      "org.tpolecat"             %% "natchez-honeycomb"                  % natchezVersion,
      "org.tpolecat"             %% "natchez-http4s"                     % natchezHttp4sVersion,
      "org.tpolecat"             %% "natchez-log"                        % natchezVersion,
      "org.tpolecat"             %% "skunk-core"                         % skunkVersion,
      "org.tpolecat"             %% "skunk-circe"                        % skunkVersion,
      "com.lihaoyi"              %% "pprint"                             % pprintVersion,
      "com.dimafeng"             %% "testcontainers-scala-munit"         % testcontainersScalaVersion % Test,
      "com.dimafeng"             %% "testcontainers-scala-localstack-v2" % testcontainersScalaVersion % Test,
      "com.dimafeng"             %% "testcontainers-scala-postgresql"    % testcontainersScalaVersion % Test,
      // testcontainers-scala-localstack-v2 requires both v1 and v2 of the aws sdk
      "io.circe"                 %% "circe-testing"                      % circeVersion               % Test,
      "com.amazonaws"             % "aws-java-sdk-core"                  % awsJavaSdkVersion          % Test,
      "edu.gemini"               %% "clue-http4s"                        % clueVersion                % Test,
      "org.scalameta"            %% "munit"                              % munitVersion               % Test,
      "org.scalameta"            %% "munit-scalacheck"                   % munitVersion               % Test,
      "org.typelevel"            %% "discipline-munit"                   % munitDisciplineVersion     % Test,
      "edu.gemini"               %% "lucuma-catalog-testkit"             % lucumaCoreVersion          % Test,
      "edu.gemini"               %% "lucuma-core-testkit"                % lucumaCoreVersion          % Test,
      "org.http4s"               %% "http4s-jdk-http-client"             % http4sJdkHttpClientVersion % Test,
      "org.typelevel"            %% "cats-time"                          % catsTimeVersion,
      "org.typelevel"            %% "log4cats-slf4j"                     % log4catsVersion,
      "org.typelevel"            %% "munit-cats-effect-3"                % munitCatsEffectVersion     % Test,
      "org.typelevel"            %% "paiges-core"                        % paigesVersion,
      "com.github.vertical-blank" % "sql-formatter"                      % "2.0.5"
    ),
    reStart / envVars += "PORT" -> "8082",
    reStartArgs += "serve"
  )

lazy val obscalc = project
  .in(file("modules/obscalc"))
  .dependsOn(service)
  .enablePlugins(NoPublishPlugin, JavaAppPackaging)
  .settings(
    name                        := "obscalc-service",
    projectDependencyArtifacts  := (Compile / dependencyClasspathAsJars).value,
    reStart / envVars += "PORT" -> "8082"
  )

lazy val calibrations = project
  .in(file("modules/calibrations"))
  .dependsOn(service)
  .enablePlugins(NoPublishPlugin, JavaAppPackaging)
  .settings(
    name                        := "calibrations-service",
    projectDependencyArtifacts  := (Compile / dependencyClasspathAsJars).value,
    reStart / envVars += "PORT" -> "8082"
  )

lazy val phase0 = project
  .in(file("modules/phase0"))
  .enablePlugins(NoPublishPlugin)
  .settings(
    name := "lucuma-odb-phase0",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-parse"          % catsParseVersion,
      "co.fs2"        %% "fs2-core"            % fs2Version,
      "co.fs2"        %% "fs2-io"              % fs2Version,
      "edu.gemini"    %% "lucuma-core"         % lucumaCoreVersion,
      "edu.gemini"    %% "lucuma-core-testkit" % lucumaCoreVersion      % Test,
      "org.scalameta" %% "munit"               % munitVersion           % Test,
      "org.scalameta" %% "munit-scalacheck"    % munitVersion           % Test,
      "org.typelevel" %% "munit-cats-effect-3" % munitCatsEffectVersion % Test,
      "org.typelevel" %% "discipline-munit"    % munitDisciplineVersion % Test
    )
  )
