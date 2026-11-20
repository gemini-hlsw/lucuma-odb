// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.IO
import cats.effect.Ref
import lucuma.odb.FMain
import lucuma.odb.StartupDiagnostics
import lucuma.odb.graphql.OdbSuite
import skunk.codec.numeric.int2
import skunk.syntax.all.*

import scala.concurrent.duration.*

class StartupDiagnosticsSuite extends OdbSuite:

  val validUsers = Nil

  // This ensures that diagnostics must pass in CI
  test("run diagnostics"):
    withSession: s =>
      StartupDiagnostics(s).runAllDiagnostics(true)

  // The version table is global state, so each of these changes it inside a
  // transaction that is rolled back.

  test("a database ahead of the time accounting rules only warns"):
    withSession: s =>
      s.transaction.use: xa =>
        s.execute(sql"INSERT INTO t_time_accounting_version VALUES ($int2, 'test')".command)((TimeAccounting.version.Current + 1).toShort) >>
          StartupDiagnostics(s).runAllDiagnostics(true) >>
          xa.rollback

  test("a database behind the time accounting rules fails"):
    withSession: s =>
      s.transaction.use: xa =>
        s.execute(sql"DELETE FROM t_time_accounting_version WHERE c_version = $int2".command)(TimeAccounting.version.Current) >>
          StartupDiagnostics(s).runAllDiagnostics(true).attempt.map(r => assert(r.isLeft, "diagnostics passed")) >>
          xa.rollback

  test("awaitMigrations waits until no migration is pending"):
    for
      // Two polls find a pending migration, the third finds none.
      calls  <- Ref.of[IO, Int](0)
      pending = calls.getAndUpdate(_ + 1).map(n => if n < 2 then List("V9999: next") else Nil)
      _      <- FMain.awaitMigrations(pending, 10.millis, 1.minute)
      _      <- assertIO(calls.get, 3)
    yield ()

  test("awaitMigrations retries a check that fails"):
    for
      // The first two checks fail, the third finds nothing pending.
      calls  <- Ref.of[IO, Int](0)
      pending = calls.getAndUpdate(_ + 1).flatMap: n =>
                  if n < 2 then IO.raiseError(new RuntimeException("connection refused"))
                  else IO.pure(Nil)
      _      <- FMain.awaitMigrations(pending, 10.millis, 1.minute)
      _      <- assertIO(calls.get, 3)
    yield ()

  test("awaitMigrations fails once its deadline passes"):
    FMain
      .awaitMigrations(IO.pure(List("V9999: never")), 10.millis, 100.millis)
      .attempt
      .map(r => assert(r.isLeft, "awaitMigrations returned"))
