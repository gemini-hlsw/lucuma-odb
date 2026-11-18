// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.IO
import cats.effect.Ref
import lucuma.odb.FMain
import lucuma.odb.StartupDiagnostics
import lucuma.odb.graphql.OdbSuite

import scala.concurrent.duration.*

class StartupDiagnosticsSuite extends OdbSuite:

  val validUsers = Nil

  // This ensures that diagnostics must pass in CI
  test("run diagnostics"):
    withSession: s =>
      StartupDiagnostics(s).runAllDiagnostics(true)

  test("awaitMigrations waits until no migration is pending"):
    for
      // Two polls find a pending migration, the third finds none.
      calls  <- Ref.of[IO, Int](0)
      pending = calls.getAndUpdate(_ + 1).map(n => if n < 2 then List("V9999: next") else Nil)
      _      <- FMain.awaitMigrations(pending, 10.millis)
      _      <- assertIO(calls.get, 3)
    yield ()
