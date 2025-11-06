import NativePackagerHelper._

// Please keep in alphabetical order
val awsJavaSdkVersion          = "1.12.792"
val boopickleVersion           = "1.5.0"
val bouncycastleVersion        = "1.70"
val catsEffectVersion          = "3.6.3"
val catsParseVersion           = "1.1.0"
val catsScalacheckVersion      = "0.3.2"
val catsTimeVersion            = "0.6.0"
val catsVersion                = "2.13.0"
val circeVersion               = "0.14.15"
val circeRefinedVersion        = "0.15.1"
val cirisVersion               = "3.11.1"
val clueVersion                = "0.49.0"
val declineVersion             = "2.5.0"
val dropwizardVersion          = "4.2.37"
val flywayVersion              = "11.16.0"
val fs2AwsVersion              = "6.2.0"
val fs2Version                 = "3.12.2"
val grackleVersion             = "0.25.0"
val http4sVersion              = "0.23.32"
val http4sBlazeVersion         = "0.23.17"
val http4sEmberVersion         = "0.23.32"
val http4sJdkHttpClientVersion = "0.10.0"
val jmhVersion                 = "1.37"
val jwtVersion                 = "11.0.3"
val keySemaphoreVersion        = "0.3.0-M1"
val kittensVersion             = "3.5.0"
val logbackVersion             = "1.5.19"
val log4catsVersion            = "2.7.1"
val lucumaCoreVersion          = "0.153.0"
val lucumaGraphQLRoutesVersion = "0.11.3"
val monocleVersion             = "3.3.0"
val munitVersion               = "1.2.0"
val munitCatsEffectVersion     = "2.1.0"   // check test output if you attempt to update this
val munitDisciplineVersion     = "2.0.0"   // check test output if you attempt to update this
val munitScalacheckVersion     = "1.2.0"   // check test output if you attempt to update this
val natchezHttp4sVersion       = "0.6.1"
val natchezVersion             = "0.3.7"
val paigesVersion              = "0.4.4"
val postgresVersion            = "42.7.8"
val pprintVersion              = "0.9.4"
val redis4CatsVersion          = "2.0.1"
val refinedVersion             = "0.11.3"
val skunkVersion               = "0.6.4"
val spireVersion               = "0.18.0"
val slf4jVersion               = "2.0.17"
val testcontainersScalaVersion = "0.43.0" // check test output if you attempt to update this
val weaverVersion              = "0.8.4"

ThisBuild / tlBaseVersion      := "0.52"
ThisBuild / scalaVersion       := "3.7.3"
ThisBuild / crossScalaVersions := Seq("3.7.3")
ThisBuild / scalacOptions     ++= Seq("-Xmax-inlines", "50") // Hash derivation fails with default of 32

ThisBuild / Test / fork              := false
ThisBuild / Test / parallelExecution := false

ThisBuild / watchOnTermination := { (action, cmd, times, state) =>
  val projNames = cmd
    .split(";")
    .flatMap(
      Some(_)
        .filter(_.contains("/reStart"))
        .flatMap(_.trim.split("/reStart") match {
          case Array(projName) => Some(projName)
          case _               => None
        })
    )
  projNames.foldLeft(state) { (acc, projName) =>
    val projRef = ProjectRef((ThisBuild / baseDirectory).value, projName)
    Project.extract(state).runTask(projRef / reStop, state)._1
  }
}

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
    name = Some("Validate ODB GraphQL schema changes"),
    params =
      Map(
        "name"          -> "Validate ODB Public API",
        "schema"        -> "main:modules/schema/src/main/resources/lucuma/odb/graphql/OdbSchema.graphql",
        "approve-label" -> "expected-breaking-change"
      ),
    cond = Some("github.event_name == 'pull_request' && matrix.shard == '1'")
  )

ThisBuild / githubWorkflowBuildPreamble +=
  WorkflowStep.Use(
    UseRef.Public("kamilkisiela", "graphql-inspector", "master"),
    name = Some("Validate ITC GraphQL schema changes"),
    params =
      Map(
        "name"          -> "Validate ITC Public API",
        "schema"        -> "main:itc/service/src/main/resources/graphql/itc.graphql",
        "approve-label" -> "expected-breaking-change"
      ),
    cond = Some("github.event_name == 'pull_request' && matrix.shard == '2'")
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

lazy val CheckoutFullWithLfs: WorkflowStep =
  WorkflowStep.Use(
    UseRef.Public("actions", "checkout", "v4"),
    name = Some("Checkout current branch (full)"),
    params = Map("fetch-depth" -> "0", "lfs" -> "true")
  )

ThisBuild / githubWorkflowJobSetup := {
  List(CheckoutFullWithLfs) :::
    WorkflowStep.SetupSbt ::
    WorkflowStep.SetupJava(githubWorkflowJavaVersions.value.toList) :::
    githubWorkflowGeneratedCacheSteps.value.toList
}

lazy val sbtDockerPublishLocal =
  WorkflowStep.Sbt(
    List(
      "clean",
      "ssoService/docker:publishLocal",
      "itcService/docker:publishLocal",
      "service/docker:publishLocal",
      "obscalc/docker:publishLocal",
      "calibrations/docker:publishLocal"
    ),
    name = Some("Build Docker images")
  )

lazy val systems: List[String] = List("sso", "itc", "odb")
lazy val appNames: Map[String, String] = Map(
  "sso" -> "${{ vars.HEROKU_SSO_APP_NAME || 'lucuma-sso' }}",
  "itc" -> "${{ vars.HEROKU_ITC_APP_NAME || 'itc' }}",
  "odb" -> "${{ vars.HEROKU_ODB_APP_NAME || 'lucuma-postgres-odb' }}"
)
lazy val procTypes: Map[String, List[String]] = Map(
  "sso" -> List("web"),
  "itc" -> List("web"),
  "odb" -> List("web", "obscalc", "calibration")
)
lazy val procTypeImageNames: Map[(String, String), String] = Map(
  ("sso", "web")           -> "lucuma-sso-service",
  ("itc", "web")           -> "lucuma-itc-service",
  ("odb", "web")           -> "lucuma-odb-service",
  ("odb", "obscalc")       -> "obscalc-service",
  ("odb", "calibration")   -> "calibrations-service"
)
lazy val environments: List[String] = List("dev", "staging", "production")
lazy val systemProcTypes: List[(String, String, String)] =
  for {
    system <- systems
    app    = appNames(system)
    proc   <- procTypes(system)
  } yield (system, app, proc)

lazy val herokuPush =
  WorkflowStep.Run(
    List(
      // Login
      "npm install -g heroku",
      "heroku container:login"
    ) ++
      systemProcTypes.flatMap { case (system, app, proc) =>
        val procImage: String = procTypeImageNames((system, proc))
        environments.flatMap( env =>
          List(
            s"docker tag noirlab/$procImage registry.heroku.com/$app-$env/$proc:$${{ github.sha }}",
            s"docker push registry.heroku.com/$app-$env/$proc:$${{ github.sha }}",
          )
        ) ++
        List( // Retag for easy release to dev
          s"docker tag noirlab/$procImage registry.heroku.com/$app-dev/$proc",
          s"docker push registry.heroku.com/$app-dev/$proc",
        )
      },
    name = Some("Push Docker images to Heroku")
  )

lazy val herokuRelease =
  WorkflowStep.Run(
    systems.map( system =>
      s"heroku container:release ${procTypes(system).mkString(" ")} -a ${appNames(system)}-dev -v"
    ),
    name = Some("Release dev app in Heroku")
  )

def imageShaEnvVar(system: String, proc: String): String =
  s"DOCKER_IMAGE_SHA_${system.toUpperCase}_${proc.toUpperCase}"

lazy val retrieveDockerImageShas = WorkflowStep.Run(
  systemProcTypes.map { case (system, app, proc) =>
    s"""echo "${imageShaEnvVar(system, proc)}=$$(docker inspect registry.heroku.com/$app-dev/$proc:$${{ github.sha }} --format={{.Id}})" >> $$GITHUB_ENV"""
  },
  name = Some("Get Docker image SHA")
)

def dockerImageShasObject(system: String): String = "{ " +
  procTypes(system).map ( proc =>
    s""""${proc}": "$${{ env.${imageShaEnvVar(system, proc)} }}""""
  ).mkString(", ") + " }"

lazy val recordDeploymentMetadata = WorkflowStep.Run(
  systems.flatMap( system =>
    List(
      s"""echo "Recording deployment $${{ github.sha }} for ${system.toUpperCase} to $${{ github.repository }}"""",
      s"""curl -s -X POST https://api.github.com/repos/$${{ github.repository }}/deployments -H "Authorization: Bearer $${{ secrets.GITHUB_TOKEN }}" -H "Accept: application/vnd.github+json" -d '{ "ref": "$${{ github.sha }}", "environment": "development", "description": "${system.toUpperCase} deployment to dev", "auto_merge": false, "required_contexts": [], "task": "deploy:${system.toUpperCase}", "payload": { "docker_image_shas": ${dockerImageShasObject(system)} } }' """
    )
  ),
  name = Some("Record deployment in GHA")
)

val mainCond                 = "github.ref == 'refs/heads/main'"
val geminiRepoCond           = "startsWith(github.repository, 'gemini')"
def allConds(conds: String*) = conds.mkString("(", " && ", ")")

ThisBuild / githubWorkflowAddedJobs ++= Seq(
  WorkflowJob(
    "deploy",
    "Build and publish Docker images / Deploy to Heroku",
    githubWorkflowJobSetup.value.toList :::
      sbtDockerPublishLocal ::
      herokuPush ::
      herokuRelease ::
      retrieveDockerImageShas ::
      recordDeploymentMetadata ::
      Nil,
    scalas = List(scalaVersion.value),
    javas = githubWorkflowJavaVersions.value.toList.take(1),
    cond = Some(allConds(mainCond, geminiRepoCond))
  )
)

lazy val buildInfoSettings = Seq(
  buildInfoKeys         := Seq[BuildInfoKey](
      scalaVersion,
      sbtVersion,
      git.gitHeadCommit,
      "buildDateTime"       -> System.currentTimeMillis()
    )
)

// START SSO

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
        "org.typelevel" %%% "discipline-munit"    % munitDisciplineVersion  % Test,
      )
    )

lazy val ssoBackendClient = project
  .in(file("modules/sso-backend-client"))
  .dependsOn(ssoFrontendClient.jvm)
  .settings(
    name := "lucuma-sso-backend-client",
    libraryDependencies ++= Seq(
      "com.github.jwt-scala" %% "jwt-circe"          % jwtVersion,
      "org.bouncycastle"      % "bcpkix-jdk15on"     % bouncycastleVersion,
      "org.bouncycastle"      % "bcpg-jdk15on"       % bouncycastleVersion,
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

// START ITC

lazy val itcCommonSettings = lucumaGlobalSettings ++ Seq(
  Test / parallelExecution := false // tests run fine in parallel but output is nicer this way
)

// Basic ITC model classes
lazy val itcModel = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("itc/model"))
  .settings(itcCommonSettings)
  .settings(
    name := "lucuma-itc",
    libraryDependencies ++= Seq(
      "io.circe"      %%% "circe-generic" % circeVersion,
      "io.circe"      %%% "circe-refined" % circeRefinedVersion,
      "org.typelevel" %%% "cats-core"     % catsVersion,
      "edu.gemini"    %%% "lucuma-core"   % lucumaCoreVersion,
      "eu.timepit"    %%% "refined"       % refinedVersion,
      "eu.timepit"    %%% "refined-cats"  % refinedVersion,
      "org.typelevel" %%% "kittens"       % kittensVersion
    )
  )

lazy val ocslibHash = taskKey[String]("hash of ocslib and graphql schema")
ThisBuild / ocslibHash / fileInputs += (itcService / baseDirectory).value.toGlob / "ocslib" / "*.jar"
ThisBuild / ocslibHash / fileInputs += (itcService / Compile / resourceDirectory).value.toGlob / "graphql" / "*.graphql"
ThisBuild / ocslibHash := {
  val jarFiles      = ocslibHash.inputFiles.filter(_.toString.endsWith(".jar")).sorted.map(_.toFile)
  val schemaFiles   =
    ocslibHash.inputFiles.filter(_.toString.endsWith(".graphql")).sorted.map(_.toFile)
  val allFiles      = jarFiles ++ schemaFiles
  val hashes        = allFiles.map(Hash(_))
  val describe      = ocsGitDescribe.value
  val combinedInput = hashes.toArray.flatten ++ describe.getBytes("UTF-8")

  Hash.toHex(Hash(combinedInput))
}

// OCS build info parsing
lazy val ocsBuildInfo   = taskKey[(String, String, String, Boolean)]("OCS build info")
lazy val ocsGitHash     = taskKey[String]("ocs git hash")
lazy val ocsGitBranch   = taskKey[String]("ocs git branch")
lazy val ocsGitDescribe = taskKey[String]("ocs git describe")
lazy val ocsLocal       = taskKey[Boolean]("ocs local changes")

ThisBuild / ocsBuildInfo := {
  import scala.util.matching.*

  val buildInfoFile = (itcService / baseDirectory).value / "ocslib" / "build-info.json"
  val content       = IO.read(buildInfoFile)

  def extractField(pattern: Regex, fieldName: String): String =
    pattern.findFirstMatchIn(content) match {
      case Some(m) => m.group(1)
      case None    => throw new RuntimeException(s"$fieldName not found in build-info.json")
    }

  val gitHash     = extractField(""""ocs_git_hash":\s*"([^"]+)"""".r, "ocs_git_hash")
  val gitBranch   = extractField(""""ocs_git_branch":\s*"([^"]+)"""".r, "ocs_git_branch")
  val gitDescribe = extractField(""""ocs_git_describe":\s*"([^"]+)"""".r, "ocs_git_describe")
  val local       = extractField(""""local":\s*(true|false)""".r, "local").toBoolean

  (gitHash, gitBranch, gitDescribe, local)
}

ThisBuild / ocsGitHash     := ocsBuildInfo.value._1
ThisBuild / ocsGitBranch   := ocsBuildInfo.value._2
ThisBuild / ocsGitDescribe := ocsBuildInfo.value._3
ThisBuild / ocsLocal       := ocsBuildInfo.value._4

// Contains the grackle server
lazy val itcService = project
  .in(file("itc/service"))
  .dependsOn(itcModel.jvm, binding)
  .enablePlugins(BuildInfoPlugin, LucumaDockerPlugin, JavaServerAppPackaging)
  .settings(itcCommonSettings)
  .settings(
    name                  := "lucuma-itc-service",
    description              := "ITC Server",
    scalacOptions -= "-Vtype-diffs",
    reStart / javaOptions := Seq(
      "-Dcats.effect.stackTracing=DISABLED",
      "-Dcats.effect.tracing.mode=none"
    ),
    libraryDependencies ++= Seq(
      "org.typelevel"        %% "grackle-core"          % grackleVersion,
      "org.typelevel"        %% "grackle-generic"       % grackleVersion,
      "org.typelevel"        %% "grackle-circe"         % grackleVersion,
      "edu.gemini"           %% "lucuma-graphql-routes" % lucumaGraphQLRoutesVersion,
      "org.tpolecat"         %% "natchez-honeycomb"     % natchezVersion,
      "org.tpolecat"         %% "natchez-log"           % natchezVersion,
      "org.tpolecat"         %% "natchez-http4s"        % natchezHttp4sVersion,
      "co.fs2"               %% "fs2-core"              % fs2Version,
      "edu.gemini"           %% "lucuma-core"           % lucumaCoreVersion,
      "org.typelevel"        %% "cats-core"             % catsVersion,
      "org.typelevel"        %% "cats-effect"           % catsEffectVersion,
      "is.cir"               %% "ciris"                 % cirisVersion,
      "org.typelevel"        %% "log4cats-slf4j"        % log4catsVersion,
      "org.slf4j"             % "slf4j-simple"          % slf4jVersion,
      "org.http4s"           %% "http4s-core"           % http4sVersion,
      "org.http4s"           %% "http4s-ember-server"   % http4sVersion,
      "org.http4s"           %% "http4s-ember-client"   % http4sVersion,
      "eu.timepit"           %% "refined"               % refinedVersion,
      "eu.timepit"           %% "refined-cats"          % refinedVersion,
      "dev.profunktor"       %% "redis4cats-effects"    % redis4CatsVersion,
      "dev.profunktor"       %% "redis4cats-log4cats"   % redis4CatsVersion,
      "com.lihaoyi"          %% "pprint"                % pprintVersion,
      "io.suzaku"            %% "boopickle"             % boopickleVersion,
      "io.chrisdavenport"    %% "keysemaphore"          % keySemaphoreVersion,
      "io.dropwizard.metrics" % "metrics-core"          % dropwizardVersion,
      "io.dropwizard.metrics" % "metrics-jvm"           % dropwizardVersion,
      "io.dropwizard.metrics" % "metrics-graphite"      % dropwizardVersion,
      "org.typelevel"        %% "munit-cats-effect"     % munitCatsEffectVersion % Test
    ),
    envVars ++= Map("ODB_BASE_URL" -> "https://lucuma-postgres-odb-dev.herokuapp.com"), // Used for local development
    buildInfoKeys         := Seq[BuildInfoKey](
      scalaVersion,
      sbtVersion,
      git.gitHeadCommit,
      "buildDateTime" -> System.currentTimeMillis(),
      ocslibHash,
      ocsGitHash,
      ocsGitBranch,
      ocsGitDescribe,
      ocsLocal
    ),
    // Name of the launch script
    executableScriptName     := "itc-service",
    dockerExposedPorts ++= Seq(6060),
    // Add the ocslib jars to the distribution
    Universal / mappings ++= {
      val dir = baseDirectory.value / "ocslib"
      (dir ** AllPassFilter).pair(relativeTo(dir.getParentFile))
    },
    // The heap needs to be a lot smaller than the dyno size. This may be
    // because the JVM tricks to load the 367M of old itc jar files increases the
    // `metaspace` size by that amount. It's a nice theory, at least.
    lucumaDockerHeapSubtract := 400
  )

lazy val itcClient = crossProject(JVMPlatform, JSPlatform)
  .in(file("itc/client"))
  .dependsOn(itcModel)
  .settings(itcCommonSettings)
  .settings(
    name := "lucuma-itc-client",
    libraryDependencies ++= Seq(
      "edu.gemini"    %%% "lucuma-core"       % lucumaCoreVersion,
      "org.typelevel" %%% "cats-core"         % catsVersion,
      "org.typelevel" %%% "cats-effect"       % catsEffectVersion,
      "org.http4s"    %%% "http4s-circe"      % http4sVersion,
      "org.http4s"    %%% "http4s-dsl"        % http4sVersion,
      "io.circe"      %%% "circe-literal"     % circeVersion,
      "edu.gemini"    %%% "clue-model"        % clueVersion,
      "edu.gemini"    %%% "clue-http4s"       % clueVersion,
      "edu.gemini"    %%% "clue-core"         % clueVersion,
      "io.circe"      %%% "circe-generic"     % circeVersion,
      "org.tpolecat"  %%% "natchez-http4s"    % natchezHttp4sVersion,
      "org.typelevel" %%% "spire"             % spireVersion,
      "org.typelevel" %%% "spire-extras"      % spireVersion,
      "org.typelevel" %%% "kittens"           % kittensVersion,
      "org.typelevel" %%% "munit-cats-effect" % munitCatsEffectVersion % Test,
      "com.lihaoyi"   %%% "pprint"            % pprintVersion          % Test
    )
  )

lazy val itcBenchmark = project
  .in(file("itc/benchmarks"))
  .enablePlugins(JmhPlugin, NoPublishPlugin)
  .dependsOn(itcService)
  .settings(
    libraryDependencies ++= Seq(
      "org.openjdk.jmh" % "jmh-core"                 % jmhVersion,
      "org.openjdk.jmh" % "jmh-generator-annprocess" % jmhVersion
    ),
    // Add project root system property for benchmarks
    fork       := false,
    Jmh / fork := false,
    javaOptions += s"-Dproject.root=${(ThisBuild / baseDirectory).value}",
    Jmh / javaOptions += s"-Dproject.root=${(ThisBuild / baseDirectory).value}"
  )

lazy val itcTestkit = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("itc/testkit"))
  .dependsOn(itcClient)
  .settings(itcCommonSettings)
  .settings(
    name := "lucuma-itc-testkit",
    libraryDependencies ++= Seq(
      "edu.gemini"        %%% "lucuma-core-testkit" % lucumaCoreVersion,
      "org.typelevel"     %%% "cats-testkit"        % catsVersion,
      "dev.optics"        %%% "monocle-law"         % monocleVersion,
      "org.typelevel"     %%% "spire-laws"          % spireVersion,
      "eu.timepit"        %%% "refined-scalacheck"  % refinedVersion,
      "io.circe"          %%% "circe-testing"       % circeVersion,
      "io.chrisdavenport" %%% "cats-scalacheck"     % catsScalacheckVersion
    )
  )

lazy val itcTests = project
  .in(file("itc/tests"))
  .enablePlugins(NoPublishPlugin)
  .dependsOn(itcService, itcClient.jvm, itcTestkit.jvm)
  .settings(
    name := "lucuma-itc-tests",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "munit-cats-effect"      % munitCatsEffectVersion     % Test,
      "com.lihaoyi"   %%% "pprint"                 % pprintVersion              % Test,
      "org.http4s"     %% "http4s-jdk-http-client" % http4sJdkHttpClientVersion % Test,
      "org.typelevel" %%% "log4cats-slf4j"         % log4catsVersion            % Test,
      "org.scalameta" %%% "munit"                  % munitVersion               % Test,
      "org.typelevel" %%% "discipline-munit"       % munitDisciplineVersion     % Test
    )
  )

lazy val itcLegacyTests = project
  .in(file("itc/legacy-tests"))
  .enablePlugins(NoPublishPlugin)
  .dependsOn(itcService, itcClient.jvm, itcTestkit.jvm)
  .settings(
    name := "lucuma-itc-legacy-tests",
    // Skip legacy tests unless explicitly enabled
    Test / test := {
      if (sys.env.get("RUN_LEGACY_TESTS").contains("true")) {
        (Test / test).value
      } else {
        streams.value.log.info("Skipping ITC legacy tests (set RUN_LEGACY_TESTS=true to enable)")
        ()
      }
    },
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "munit-cats-effect"      % munitCatsEffectVersion     % Test,
      "com.lihaoyi"   %%% "pprint"                 % pprintVersion              % Test,
      "org.http4s"     %% "http4s-jdk-http-client" % http4sJdkHttpClientVersion % Test,
      "org.typelevel" %%% "log4cats-slf4j"         % log4catsVersion            % Test,
      "org.scalameta" %%% "munit"                  % munitVersion               % Test,
      "org.typelevel" %%% "discipline-munit"       % munitDisciplineVersion     % Test
    )
  )

// START ODB

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
        "org.scalameta" %%% "munit-scalacheck"           % munitScalacheckVersion % Test,
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
  .dependsOn(schema.jvm, itcClient.jvm, itcTestkit.jvm)
  .enablePlugins(NoPublishPlugin)
  .settings(
    name := "lucuma-odb-sequence",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit"              % munitVersion           % Test,
      "org.scalameta" %% "munit-scalacheck"   % munitScalacheckVersion % Test,
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
      "org.scalameta" %% "munit-scalacheck"    % munitScalacheckVersion % Test,
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
      "edu.gemini"               %% "lucuma-horizons"                    % lucumaCoreVersion,
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
      "org.scalameta"            %% "munit-diff"                         % munitVersion               % Test,
      "org.scalameta"            %% "munit-scalacheck"                   % munitScalacheckVersion     % Test,
      "org.typelevel"            %% "discipline-munit"                   % munitDisciplineVersion     % Test,
      "edu.gemini"               %% "lucuma-catalog-testkit"             % lucumaCoreVersion          % Test,
      "edu.gemini"               %% "lucuma-core-testkit"                % lucumaCoreVersion          % Test,
      "org.http4s"               %% "http4s-jdk-http-client"             % http4sJdkHttpClientVersion % Test,
      "org.typelevel"            %% "cats-time"                          % catsTimeVersion,
      "org.typelevel"            %% "log4cats-slf4j"                     % log4catsVersion,
      "org.typelevel"            %% "munit-cats-effect"                  % munitCatsEffectVersion % Test,
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
      "org.scalameta" %% "munit-scalacheck"    % munitScalacheckVersion % Test,
      // "org.typelevel" %% "munit-cats-effect-3" % munitCatsEffectVersion % Test,
      "org.typelevel" %% "munit-cats-effect"   % munitCatsEffectVersion % Test,
      "org.typelevel" %% "discipline-munit"    % munitDisciplineVersion % Test
    )
  )
