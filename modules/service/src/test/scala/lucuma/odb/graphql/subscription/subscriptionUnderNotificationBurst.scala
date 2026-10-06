// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package subscription

import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all.*
import io.circe.Json
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.FMain
import lucuma.odb.data.EditType
import lucuma.odb.service.Services
import lucuma.odb.service.UserService
import lucuma.odb.util.Codecs.observation_id
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.otel4s.trace.TracerProvider
import skunk.codec.all.*
import skunk.implicits.*

import scala.concurrent.duration.*

/**
 * An observation edit followed, in the same transaction, by more notifications
 * than a feed queues must still reach the topic. Feeds that queried the LISTEN
 * session used to deadlock here (see `OdbTopic.Sessions`).
 *
 * A deadlocked feed cannot be cancelled, so this suite runs no GraphQL server
 * (its feeds would deadlock too and hang the teardown) and releases the topics
 * under test only when they delivered.
 */
class subscriptionUnderNotificationBurst extends OdbSuite {

  val pi = TestUsers.Standard.pi(1, 30)

  def validUsers = List(pi)

  override def munitFixtures = List(sessionFixture)

  // Well above the 1024 notifications a feed queues.
  private val BurstSize: Int = 4000

  private val Patience: FiniteDuration = 30.seconds

  // The server fixture would have done this on authentication.
  private def canonicalize(user: User): IO[Unit] =
    given Tracer[IO] = Tracer.noop
    withSession(s => Services.asSuperUser(UserService.fromSession(s).canonicalizeUser(user)))

  private def mutate(user: User, document: String): IO[Json] =
    queryWithSqlStats(user, document).map(_._1)

  private def createProgram(user: User): IO[Program.Id] =
    mutate(user, """mutation { createProgram(input: { SET: { name: "burst" } }) { program { id } } }""")
      .map(_.hcursor.downFields("data", "createProgram", "program", "id").require[Program.Id])

  private def createObservation(user: User, pid: Program.Id): IO[Observation.Id] =
    mutate(user, s"""mutation { createObservation(input: { programId: "${pid.show}", SET: { subtitle: "before" } }) { observation { id } } }""")
      .map(_.hcursor.downFields("data", "createObservation", "observation", "id").require[Observation.Id])

  private def topics: Resource[IO, OdbMapping.Topics[IO]] =
    given Tracer[IO]         = Tracer.noop
    given TracerProvider[IO] = TracerProvider.noop
    given MeterProvider[IO]  = MeterProvider.noop
    FMain.databasePoolResource[IO](databaseConfig).flatMap(OdbMapping.Topics[IO](_))

  private val listeningBackends: IO[Set[Int]] =
    withSession: s =>
      s.execute(
        sql"""
          SELECT pid
          FROM   pg_stat_activity
          WHERE  datname = current_database()
          AND    query LIKE 'LISTEN %'
        """.query(int4)
      ).map(_.toSet)

  // The feeds start in the background.
  private def awaitListener(before: Set[Int]): IO[Unit] =
    listeningBackends
      .flatMap: now =>
        (IO.sleep(250.millis) >> awaitListener(before)).whenA((now -- before).isEmpty)
      .timeoutTo(Patience, IO.raiseError(new RuntimeException("The topic feeds never started listening.")))

  // Postgres drops duplicate notifications within a transaction, hence the distinct ids.
  private def editThenBurst(pid: Program.Id, oid: Observation.Id): IO[Unit] =
    withSession: s =>
      s.transaction.use: _ =>
        s.execute(
          sql"UPDATE t_observation SET c_subtitle = 'burst' WHERE c_observation_id = $observation_id".command
        )(oid) >>
        s.unique(
          sql"""
            SELECT count(pg_notify(
              'ch_obscalc_update',
              'o-' || to_hex(i) || ',' || '#${pid.toString}' || ',pending,calculating,undefined,undefined,UPDATE'
            ))
            FROM generate_series(1, #${BurstSize.toString}) AS i
          """.query(int8)
        ).void

  test("deliver an observation edit that is followed by a notification burst"):
    for
      _                 <- canonicalize(pi)
      pid               <- createProgram(pi)
      oid               <- createObservation(pi, pid)
      before            <- listeningBackends
      (topics, release) <- topics.allocated
      _                 <- awaitListener(before)
      edit              <- topics.observation
                             .subscribeAwait(16)
                             .use: events =>
                               editThenBurst(pid, oid) >>
                                 events
                                   .find(_.observationId === oid)
                                   .compile
                                   .lastOrError
                                   .timeoutTo(
                                     Patience,
                                     IO.raiseError(new RuntimeException(s"No observation edit for $oid within $Patience; the topic feed is stuck."))
                                   )
                             .attempt
      _                 <- release.whenA(edit.isRight)
      edit              <- IO.fromEither(edit)
    yield assertEquals(edit.editType, EditType.Updated)

}
