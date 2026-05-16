// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import com.dimafeng.testcontainers.GenericContainer
import com.dimafeng.testcontainers.PostgreSQLContainer
import org.testcontainers.containers.PostgreSQLContainer.POSTGRESQL_PORT
import org.testcontainers.containers.wait.strategy.Wait
import org.testcontainers.images.builder.ImageFromDockerfile

import java.nio.file.Paths
import scala.jdk.CollectionConverters.*

/**
 * A single PostgreSQL container shared across the entire test JVM. Tests do not connect
 * to the migrated "template" database directly — instead, each suite calls
 * [[createFreshDb]] to clone it via `CREATE DATABASE … TEMPLATE …`, which is dramatically
 * faster than starting a fresh container per suite.
 */
object SharedOdbContainer {

  private val AdminUser     = PostgreSQLContainer.defaultUsername
  private val AdminPassword = PostgreSQLContainer.defaultPassword
  // The migrated DB from the Docker image. Used as the template; tests never connect to it.
  private val TemplateDb    = PostgreSQLContainer.defaultDatabaseName

  private lazy val started: GenericContainer = {
    val env = Map(
      "POSTGRES_USER"     -> AdminUser,
      "POSTGRES_PASSWORD" -> AdminPassword,
      "POSTGRES_DB"       -> TemplateDb
    )

    // sbt and vscode-with-bloop start at the repo root; vscode-with-sbt starts at modules/service.
    val dockerPrefix = Paths.get("modules", "service")
    val dockerSuffix = Paths.get("src", "Dockerfile")
    val dockerPath   =
      if (Paths.get(".").toAbsolutePath.normalize.endsWith(dockerPrefix)) dockerSuffix
      else dockerPrefix.resolve(dockerSuffix)

    val image = new ImageFromDockerfile("lucuma-odb-test-db")
      .withDockerfile(dockerPath)
      .withBuildArgs(env.asJava)

    val c = GenericContainer(
      image,
      env          = env,
      exposedPorts = Seq(POSTGRESQL_PORT),
      waitStrategy = Wait
        .forLogMessage(".*database system is ready to accept connections.*", 1)
        .withStartupTimeout(java.time.Duration.ofSeconds(30))
    )
    c.start()

    // Mark the migrated DB as a template and forbid direct connections, so cloning
    // is always allowed and accidental connections are caught early.
    psql(
      c,
      s"UPDATE pg_database SET datistemplate = true, datallowconn = false WHERE datname = '$TemplateDb';"
    )

    val hook = new Thread(() => try c.stop() catch { case _: Throwable => () })
    Runtime.getRuntime.addShutdownHook(hook)
    c
  }

  def host: String = started.containerIpAddress
  def port: Int    = started.mappedPort(POSTGRESQL_PORT)

  /** Create a fresh database cloned from the migrated template. Returns the new DB name. */
  def createFreshDb(): String = {
    val name = s"odb_test_${java.util.UUID.randomUUID().toString.replace("-", "")}"
    psql(started, s"""CREATE DATABASE "$name" TEMPLATE "$TemplateDb";""")
    name
  }

  def dropDb(name: String): Unit = {
    psql(
      started,
      s"""SELECT pg_terminate_backend(pid) FROM pg_stat_activity
         |WHERE datname = '$name' AND pid <> pg_backend_pid();""".stripMargin
    )
    psql(started, s"""DROP DATABASE IF EXISTS "$name";""")
  }

  private def psql(c: GenericContainer, sql: String): Unit = {
    val result = c.container.execInContainer(
      "psql", "-U", AdminUser, "-d", "postgres", "-v", "ON_ERROR_STOP=1", "-c", sql
    )
    if (result.getExitCode != 0)
      throw new RuntimeException(
        s"psql failed (exit=${result.getExitCode}): ${result.getStderr}\nSQL: $sql"
      )
  }
}
