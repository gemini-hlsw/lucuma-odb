// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.otel.health

import cats.effect.Ref
import cats.effect.Resource
import cats.effect.Temporal
import cats.effect.syntax.all.*
import cats.syntax.all.*
import org.typelevel.log4cats.Logger
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.metrics.Meter
import skunk.Session
import skunk.codec.numeric.int4
import skunk.codec.numeric.int8
import skunk.codec.text.text
import skunk.implicits.*

import scala.concurrent.duration.*

/**
 * Database-wide connection usage exported to grafana via otel4s.
 */
object DbConnections:

  private val UsedMetricName: String = "lucuma.db.connections.used"
  private val MaxMetricName: String  = "lucuma.db.connections.max"

  private final case class Snapshot(max: Int, byApplication: List[(String, Long)])

  private val SelectMax =
    sql"SELECT current_setting('max_connections')::int4".query(int4)

  // Not `backend_type`: it reads null for other roles' sessions without
  // pg_read_all_stats. `datid` is always visible and excludes background processes.
  private val SelectByApplication =
    sql"""
      SELECT COALESCE(NULLIF(application_name, ''), 'unknown'), count(*)
      FROM   pg_stat_activity
      WHERE  datid IS NOT NULL
      GROUP  BY 1
    """.query(text ~ int8)

  private def snapshot[F[_]: Temporal](session: Session[F]): F[Snapshot] =
    (session.unique(SelectMax), session.execute(SelectByApplication)).mapN(Snapshot.apply)

  /**
   * Samples on a session every `period`. A failed or timed-out sample clears
   * the gauges, so Grafana sees no data rather than stale numbers.
   */
  def resource[F[_]: Temporal as T: Logger as L: Meter as M](
    session: Resource[F, Session[F]],
    period:  FiniteDuration,
    timeout: FiniteDuration = 10.seconds
  ): Resource[F, Unit] =
    for
      last <- Resource.eval(Ref[F].of(none[Snapshot]))
      _    <- M.observableGauge[Long](UsedMetricName)
               .withUnit("{connection}")
               .withDescription("Open client connections to the database, by application_name")
               .createWithCallback: cb =>
                  last.get.flatMap(_.traverse_(_.byApplication.traverse_ { (app, n) =>
                    cb.record(n, Attribute("application_name", app))
                  }))
      _    <- M.observableGauge[Long](MaxMetricName)
               .withUnit("{connection}")
               .withDescription("The database server's max_connections")
               .createWithCallback: cb =>
                  last.get.flatMap(_.traverse_(s => cb.record(s.max.toLong)))
      sample = session.use(snapshot[F]).timeout(timeout).attempt.flatMap:
                 case Right(s) => last.set(s.some)
                 case Left(e)  => L.warn(e)("Database connection sample failed") >> last.set(none)
      _    <- (sample >> T.sleep(period)).foreverM.background
    yield ()
