// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.effect.IO
import cats.syntax.all.*
import skunk.codec.all.*
import skunk.implicits.*

import scala.concurrent.duration.*

/**
 * Verifies that requests keep working after the server's database connections die. The user
 * service used to pin one pooled session for the life of the server, so after a database restart
 * every request failed canonicalizing its user until the service was bounced.
 */
class queryAfterConnectionLoss extends OdbSuite:

  val httpPi = TestUsers.Standard.pi(1, 30)
  val wsPi   = TestUsers.Standard.pi(2, 31)

  def validUsers = List(httpPi, wsPi)

  private val serverBackends: IO[List[Int]] =
    withSession: s =>
      s.execute(
        sql"""
          SELECT pid
          FROM   pg_stat_activity
          WHERE  datname = current_database()
          AND    pid <> pg_backend_pid()
          AND    application_name = 'odb-web'
        """.query(int4)
      )

  private val loseConnections: IO[Unit] =
    for
      pids <- serverBackends
      _    <- IO.raiseWhen(pids.isEmpty):
                new RuntimeException("Expected server connections to terminate, found none.")
      _    <- withSession: s =>
                pids.traverse_(pid => s.unique(sql"SELECT pg_terminate_backend($int4)".query(bool))(pid).void)
      _    <- awaitGone(pids.toSet)
    yield ()

  // skunk only marks a session unhealthy once its reader has seen the EOF, so wait for
  // the backends to disappear rather than sleeping a fixed time.
  private def awaitGone(pids: Set[Int]): IO[Unit] =
    def go: IO[Unit] =
      serverBackends.flatMap: live =>
        if live.exists(pids) then IO.sleep(100.millis) *> go else IO.unit
    go.timeoutTo(
      30.seconds,
      IO.raiseError(new RuntimeException("Terminated server connections never went away."))
    )

  test("serve new users over HTTP and WS after the database connections are lost"):
    for
      _ <- loseConnections
      _ <- createProgramAs(httpPi, "after loss (http)", ClientOption.Http)
      _ <- createProgramAs(wsPi, "after loss (ws)", ClientOption.Ws)
    yield ()
