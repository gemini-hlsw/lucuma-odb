val awsJavaSdkVersion          = "1.12.787"
val catsParseVersion           = "1.1.0"
val catsTimeVersion            = "0.6.0"
val circeVersion               = "0.14.14"
val circeRefinedVersion        = "0.15.1"
val cirisVersion               = "3.10.0"
val clueVersion                = "0.46.0"
val declineVersion             = "2.5.0"
val disciplineMunitVersion     = "1.0.9"
val flywayVersion              = "9.22.3"
val fs2AwsVersion              = "6.2.0"
val fs2Version                 = "3.12.0"
val grackleVersion             = "0.25.0"
val http4sBlazeVersion         = "0.23.17"
val http4sEmberVersion         = "0.23.30"
val http4sJdkHttpClientVersion = "0.10.0"
val jwtVersion                 = "10.0.4"
val bouncycastleVersion        = "1.81"
val weaverVersion              = "0.8.4"
val logbackVersion             = "1.5.18"
val log4catsVersion            = "2.7.1"
val lucumaItcVersion           = "0.43.0"
val lucumaCoreVersion          = "0.142.0"
val lucumaGraphQLRoutesVersion = "0.10.3"
val munitVersion               = "0.7.29"  // check test output if you attempt to update this
val munitCatsEffectVersion     = "1.0.7"   // check test output if you attempt to update this
val munitDisciplineVersion     = "1.0.9"   // check test output if you attempt to update this
val natchezHttp4sVersion       = "0.6.1"
val natchezVersion             = "0.3.7"
val paigesVersion              = "0.4.4"
val postgresVersion            = "42.7.7"
val pprintVersion              = "0.9.3"
val skunkVersion               = "0.6.4"
val slf4jVersion               = "2.0.17"
val testcontainersScalaVersion = "0.40.14" // check test output if you attempt to update this

ThisBuild / tlBaseVersion      := "0.28"
ThisBuild / scalaVersion       := "3.7.2"
ThisBuild / crossScalaVersions := Seq("3.7.2")

ThisBuild / Test / fork              := false
ThisBuild / Test / parallelExecution := false

val herokuToken = "HEROKU_API_KEY" -> "${{ secrets.HEROKU_API_KEY }}"
ThisBuild / githubWorkflowEnv += herokuToken

ThisBuild / githubWorkflowSbtCommand := "sbt -v -J-Xmx6g"

ThisBuild / githubWorkflowBuildPreamble ~= { steps =>
  Seq(
    WorkflowStep.Run(List("chmod 600 test-cert/server.key"), name = Some("Set up cert permissions (1)")),
    WorkflowStep.Run(List("sudo chown 999 test-cert/server.key"), name = Some("Set up cert permissions (2)")),
  ) ++ steps
}

ThisBuild / githubWorkflowBuildPreamble +=
  WorkflowStep.Use(
    UseRef.Public("gemini-hlsw", "migration-validator-action", "main"),
    name = Some("Validate Migrations"),
    params = Map("path" -> "modules/service/src/main/resources/db/migration/"),
    cond = Some("github.event_name == 'pull_request'  && matrix.shard == '1'")
  )

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

lazy val sbtDockerPublishLocal =
  WorkflowStep.Sbt(
    List(
      "clean",
      "service/docker:publishLocal",
      "obscalc/docker:publishLocal",
      "calibrations/docker:publishLocal",
      "ssoService/docker:publishLocal"
    ),
    name = Some("Build Docker images")
  )

lazy val herokuOdbAppName = "${{ vars.HEROKU_ODB_APP_NAME || 'lucuma-postgres-odb-dev' }}"
lazy val herokuSsoAppName = "${{ vars.HEROKU_SSO_APP_NAME || 'lucuma-sso-dev' }}"

lazy val herokuPush =
  WorkflowStep.Run(
    List(
      // Login
      "npm install -g heroku",
      "heroku container:login",
      // Tag
      s"docker tag noirlab/lucuma-sso-service registry.heroku.com/$herokuSsoAppName/web",
      s"docker tag noirlab/lucuma-odb-service registry.heroku.com/$herokuOdbAppName/web",
      s"docker tag noirlab/obscalc-service registry.heroku.com/$herokuOdbAppName/obscalc",
      s"docker tag noirlab/calibrations-service registry.heroku.com/$herokuOdbAppName/calibration",
      // Push
      s"docker push registry.heroku.com/$herokuSsoAppName/web",
      s"docker push registry.heroku.com/$herokuOdbAppName/web",
      s"docker push registry.heroku.com/$herokuOdbAppName/obscalc",
      s"docker push registry.heroku.com/$herokuOdbAppName/calibration",
    ),
    name = Some("Push Docker images to Heroku")
  )

lazy val herokuRelease =
  WorkflowStep.Run(
    List(
      s"heroku container:release web -a $herokuSsoAppName -v",
      s"heroku container:release web obscalc calibration -a $herokuOdbAppName -v"
    ),
    name = Some("Release dev app in Heroku")
  )

val mainCond                 = "github.ref == 'refs/heads/main'"
val geminiRepoCond           = "startsWith(github.repository, 'gemini')"
def allConds(conds: String*) = conds.mkString("(", " && ", ")")

ThisBuild / githubWorkflowAddedJobs +=
  WorkflowJob(
    "deploy",
    "Build and publish Docker images / Deploy to Heroku",
    githubWorkflowJobSetup.value.toList :::
      sbtDockerPublishLocal ::
      herokuPush ::
      herokuRelease ::
      Nil,
    scalas = List(scalaVersion.value),
    javas = githubWorkflowJavaVersions.value.toList.take(1),
    cond = Some(allConds(mainCond, geminiRepoCond))
  )

lazy val buildInfoSettings = Seq(
  buildInfoKeys         := Seq[BuildInfoKey](
      scalaVersion,
      sbtVersion,
      git.gitHeadCommit,
      "buildDateTime"       -> System.currentTimeMillis()
    )
)

lazy val ssoFrontendClient =
  crossProject(JVMPlatform, JSPlatform)
    .crossType(CrossType.Pure)
    .in(file("modules/sso-frontend-client"))
    .settings(
      name := "lucuma-sso-frontend-client",
      libraryDependencies ++= Seq(
        "io.circe"      %%% "circe-core"          % circeVersion,
        "io.circe"      %%% "circe-generic"       % circeVersion,
        "io.circe"      %%% "circe-parser"        % circeVersion,
        "io.circe"      %%% "circe-refined"       % circeRefinedVersion,
        "edu.gemini"    %%% "lucuma-core"         % lucumaCoreVersion,
        "edu.gemini"    %%% "lucuma-core-testkit" % lucumaCoreVersion,
        "org.scalameta" %%% "munit"               % munitVersion % Test,
        "org.typelevel" %%% "discipline-munit"    % disciplineMunitVersion  % Test,
      )
    )

lazy val ssoBackendClient = project
  .in(file("modules/sso-backend-client"))
  .dependsOn(ssoFrontendClient.jvm)
  .settings(
    name := "lucuma-sso-backend-client",
    libraryDependencies ++= Seq(
      "com.github.jwt-scala" %% "jwt-circe"          % jwtVersion,
      "org.bouncycastle"      % "bcpkix-jdk18on"     % bouncycastleVersion,
      "org.bouncycastle"      % "bcpg-jdk18on"       % bouncycastleVersion,
      "org.http4s"           %% "http4s-core"        % http4sEmberVersion,
      "org.http4s"           %% "http4s-client"      % http4sEmberVersion,
      "org.http4s"           %% "http4s-dsl"         % http4sEmberVersion,
      "org.tpolecat"         %% "natchez-core"       % natchezVersion,
      "org.typelevel"        %% "log4cats-core"      % log4catsVersion
    )
  )

lazy val ssoService = project
  .in(file("modules/sso-service"))
  .dependsOn(ssoBackendClient)
  .enablePlugins(NoPublishPlugin, LucumaDockerPlugin, JavaAppPackaging, BuildInfoPlugin)
  .settings(buildInfoSettings)
  .settings(
    name := "lucuma-sso-service",
    libraryDependencies ++= Seq(
      "org.typelevel"       %% "grackle-skunk"           % grackleVersion,
      "org.tpolecat"        %% "skunk-core"              % skunkVersion,
      "org.tpolecat"        %% "skunk-circe"             % skunkVersion,
      "org.flywaydb"         % "flyway-core"             % flywayVersion,
      "org.postgresql"       % "postgresql"              % postgresVersion,
      "org.http4s"          %% "http4s-blaze-server"     % http4sBlazeVersion,
      "org.http4s"          %% "http4s-ember-client"     % http4sEmberVersion,
      "org.http4s"          %% "http4s-circe"            % http4sEmberVersion,
      "org.http4s"          %% "http4s-dsl"              % http4sEmberVersion,
      "is.cir"              %% "ciris"                   % cirisVersion,
      "com.monovore"        %% "decline-effect"          % declineVersion,
      "org.typelevel"       %% "log4cats-slf4j"          % log4catsVersion,
      "ch.qos.logback"       % "logback-classic"         % logbackVersion,
      "io.circe"            %% "circe-generic"           % circeVersion,
      "org.tpolecat"        %% "natchez-honeycomb"       % natchezVersion,
      "org.tpolecat"        %% "natchez-http4s"          % natchezHttp4sVersion,
      "org.tpolecat"        %% "natchez-log"             % natchezVersion,
      "edu.gemini"          %% "lucuma-graphql-routes" % lucumaGraphQLRoutesVersion,
      "io.circe"            %% "circe-literal"         % circeVersion  % Test,
      "com.disneystreaming" %% "weaver-cats"           % weaverVersion % Test,
      "com.disneystreaming" %% "weaver-scalacheck"     % weaverVersion % Test
    ),
    testFrameworks += new TestFramework("weaver.framework.CatsEffect"),
    reStart / envVars += "PORT" -> "8082",
    reStartArgs       += "serve",
    description                     := "Lucuma SSO Service",
    // Name of the launch script
    executableScriptName            := "lucuma-sso-service",
    dockerExposedPorts ++= Seq(8082),
    // Truncate DYNO on first dot. For web dyno, execute "serve", otherwise execute whatever the dyno type is (eg: "create-service-user" or "create-jwt").
    bashScriptExtraDefines += """DYNO_TYPE=${DYNO%%.*}; if [[ "$DYNO_TYPE" == "web" ]]; then set -- serve; else set; set -- $DYNO_TYPE $1 $2; fi"""
  )

lazy val ssoBackendExample = project
  .in(file("modules/sso-backend-example"))
  .enablePlugins(NoPublishPlugin)
  .dependsOn(ssoBackendClient)
  .settings(
    name := "lucuma-sso-backend-example",
    libraryDependencies ++= Seq(
      "is.cir"       %% "ciris"               % cirisVersion,
      "org.http4s"   %% "http4s-ember-client" % http4sEmberVersion,
      "org.http4s"   %% "http4s-ember-server" % http4sEmberVersion,
      "org.slf4j"    %  "slf4j-simple"        % slf4jVersion,
      "org.tpolecat" %% "natchez-http4s"      % natchezHttp4sVersion,
      "org.tpolecat" %% "natchez-honeycomb"   % natchezVersion,
    )
  )

lazy val schema =
  crossProject(JVMPlatform, JSPlatform)
    .crossType(CrossType.Pure)
    .dependsOn(ssoFrontendClient)
    .in(file("modules/schema"))
    .settings(
      name := "lucuma-odb-schema",
      libraryDependencies ++= Seq(
        "io.circe"      %%% "circe-parser"               % circeVersion,
        "io.circe"      %%% "circe-literal"              % circeVersion,
        "io.circe"      %%% "circe-refined"              % circeRefinedVersion,
        "edu.gemini"    %%% "lucuma-core"                % lucumaCoreVersion,
        "io.circe"      %%% "circe-testing"              % circeVersion           % Test,
        "edu.gemini"    %%% "lucuma-core-testkit"        % lucumaCoreVersion      % Test,
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
  .dependsOn(binding, phase0, sequence, smartgcal, ssoFrontendClient.jvm, ssoBackendClient)
  .enablePlugins(NoPublishPlugin, LucumaDockerPlugin, JavaAppPackaging, BuildInfoPlugin)
  .settings(buildInfoSettings)
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
      "is.cir"                   %% "ciris"                              % cirisVersion,
      "is.cir"                   %% "ciris-refined"                      % cirisVersion,
      "org.flywaydb"              % "flyway-core"                        % flywayVersion,
      "org.http4s"               %% "http4s-blaze-server"                % http4sBlazeVersion,
      "org.http4s"               %% "http4s-ember-client"                % http4sEmberVersion,
      "org.postgresql"            % "postgresql"                         % postgresVersion,
      "org.tpolecat"             %% "natchez-honeycomb"                  % natchezVersion,
      "org.tpolecat"             %% "natchez-http4s"                     % natchezHttp4sVersion,
      "org.tpolecat"             %% "natchez-log"                        % natchezVersion,
      "org.tpolecat"             %% "natchez-noop"                       % natchezVersion,
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
    reStartArgs += "serve",
    description                     := "Lucuma ODB Service",
    // Add command line parameters
    bashScriptExtraDefines += """set -- -Dfile.encoding=UTF-8 serve""",
    // Name of the launch script
    executableScriptName            := "lucuma-odb-service",
    dockerExposedPorts ++= Seq(8082)
  )
  

lazy val obscalc = project
  .in(file("modules/obscalc"))
  .dependsOn(service)
  .enablePlugins(NoPublishPlugin, LucumaDockerPlugin, JavaAppPackaging)
  .settings(
    name                        := "obscalc-service",
    projectDependencyArtifacts  := (Compile / dependencyClasspathAsJars).value,
    reStart / envVars += "PORT" -> "8082",
    description                     := "Lucuma ODB ObsCalc Service",
    // Add command line parameters
    bashScriptExtraDefines += """set -- -Dfile.encoding=UTF-8""",
    // Name of the launch script
    executableScriptName            := "lucuma-odb-obscalc-service",
    dockerExposedPorts ++= Seq(8082)
  )

lazy val calibrations = project
  .in(file("modules/calibrations"))
  .dependsOn(service)
  .enablePlugins(NoPublishPlugin, LucumaDockerPlugin, JavaAppPackaging)
  .settings(
    name                        := "calibrations-service",
    projectDependencyArtifacts  := (Compile / dependencyClasspathAsJars).value,
    reStart / envVars += "PORT" -> "8082",
    description                     := "Lucuma ODB Calibrations Service",
    // Add command line parameters
    bashScriptExtraDefines += """set -- -Dfile.encoding=UTF-8""",
    // Name of the launch script
    executableScriptName            := "lucuma-odb-calibrations-service",
    dockerExposedPorts ++= Seq(8082)
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
