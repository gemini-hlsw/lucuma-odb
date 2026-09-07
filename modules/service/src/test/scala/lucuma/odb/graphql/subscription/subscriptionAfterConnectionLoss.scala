// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package subscription

import cats.effect.IO
import cats.syntax.all.*
import io.circe.literal.*
import skunk.codec.all.*
import skunk.implicits.*

import scala.concurrent.duration.*

/**
 * Verifies that subscriptions keep working after the database connection that
 * `OdbTopic.runFeeds` holds for LISTEN dies.  Before the feeds were made
 * restartable the service went permanently deaf here, and had to be bounced.
 */
class subscriptionAfterConnectionLoss extends OdbSuite with SubscriptionUtils {

  val pi = TestUsers.Standard.pi(1, 30)

  def validUsers = List(pi)

  // The topic feeds all share one session, whose last statement is a LISTEN.
  private val listeningBackends: IO[List[Int]] =
    withSession: s =>
      s.execute(
        sql"""
          SELECT pid
          FROM   pg_stat_activity
          WHERE  datname = current_database()
          AND    pid <> pg_backend_pid()
          AND    query LIKE 'LISTEN %'
        """.query(int4)
      )

  private def terminate(pids: List[Int]): IO[Unit] =
    withSession: s =>
      pids.traverse_ : pid =>
        s.unique(sql"SELECT pg_terminate_backend($int4)".query(bool))(pid).void

  private def awaitReconnect(oldPids: Set[Int]): IO[Int] =
    def go: IO[Int] =
      listeningBackends
        .map(_.find(pid => !oldPids(pid)))
        .flatMap:
          case Some(pid) => IO.pure(pid)
          case None      => IO.sleep(250.millis) *> go
    go.timeoutTo(
      30.seconds,
      IO.raiseError(new RuntimeException("Topic feeds never reconnected after the connection was lost."))
    )

  /**
   * Kills the LISTEN connection and waits for the feeds to come back on a new
   * one.  Raises rather than returning quietly when there is nothing to kill,
   * so that the recovery assertion can never pass vacuously.
   */
  private val loseConnection: IO[Unit] =
    for
      before <- listeningBackends
      _      <- IO.raiseWhen(before.isEmpty):
                  new RuntimeException("Expected a LISTEN connection to terminate, found none.")
      _      <- terminate(before)
      after  <- awaitReconnect(before.toSet)
      _      <- IO.println(s"*** ----- topic feeds reconnected: ${before.mkString(",")} -> $after")
    yield ()

  test("keep delivering events after the LISTEN connection is lost"):
    subscriptionExpect(
      user  = pi,
      query = """
        subscription {
          programEdit {
            editType
            value { name }
          }
        }
      """,
      mutations = Right(loseConnection >> createProgram(pi, "reconnected")),
      expected = List(
        json"""{ "programEdit": { "editType": "CREATED", "value": { "name": "reconnected" } } }""",
        json"""{ "programEdit": { "editType": "UPDATED", "value": { "name": "reconnected" } } }"""
      )
    )

}
