// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.resource

import cats.effect.*
import lucuma.resource.test.DatabaseFixtures
import munit.Location
import org.typelevel.log4cats.testing.TestingLogger
import resource.server.http4s.ResourceMain
import skunk.Command
import skunk.Void
import skunk.syntax.all.*

class StartupDiagnosticsSuite extends DatabaseFixtures:

  // The tests share one container, so each one undoes its break.
  private def withBrokenSchema(breakIt: Command[Void], undo: Command[Void])(
    body: IO[Unit]
  ): IO[Unit] =
    Resource
      .make(session.use(_.execute(breakIt)).void)(_ => session.use(_.execute(undo)).void)
      .surround(body)

  private def expectStartupRefused(expectedErrors: List[String])(using Location): IO[Unit] =
    given logger: TestingLogger[IO] = TestingLogger.impl[IO]()
    ResourceMain
      .runStartupDiagnostics[IO](databaseConfig)
      .attempt
      .flatMap: result =>
        logger.logged.map: logged =>
          assert(result.isLeft, "Expected startup diagnostics to fail.")
          val errors = logged.collect { case TestingLogger.ERROR(message, _) => message }.toList
          assertEquals(errors, expectedErrors :+ "Startup diagnostics failed.")

  // This ensures that diagnostics must pass in CI
  test("diagnostics pass against the clean schema"):
    ResourceMain.runStartupDiagnostics[IO](databaseConfig)

  test("a renamed enum label stops startup"):
    withBrokenSchema(
      sql"ALTER TYPE e_too_support RENAME VALUE 'Rapid' TO 'Quick'".command,
      sql"ALTER TYPE e_too_support RENAME VALUE 'Quick' TO 'Rapid'".command
    ):
      expectStartupRefused(
        List(
          "E: unexpected tag \"Quick\" was found in the database schema.",
          "E: expected tag \"Rapid\" was not found in the database schema."
        )
      )

  test("a Postgres enum that no check covers stops startup"):
    withBrokenSchema(
      sql"CREATE TYPE e_unchecked AS ENUM ('a')".command,
      sql"DROP TYPE e_unchecked".command
    ):
      expectStartupRefused(List("Postgres enum e_unchecked has not been checked."))
