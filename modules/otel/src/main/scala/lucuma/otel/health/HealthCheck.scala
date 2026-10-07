// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.otel.health

import cats.effect.Concurrent
import cats.effect.Resource
import cats.syntax.all.*
import org.http4s.Method
import org.http4s.Request
import org.http4s.Uri
import org.http4s.client.Client
import skunk.Session
import skunk.codec.numeric.int4
import skunk.implicits.*

enum Importance derives CanEqual:
  /** A failure makes the service unable to do its job, readiness reports 503. */
  case Required
  /** A failure degrades a feature but the service keeps serving, readiness stays 200. */
  case Info

/** A named dependency probe. `run` succeeds when the dependency is reachable and raises otherwise. */
final case class HealthCheck[F[_]](
  name:       String,
  importance: Importance,
  run:        F[Unit]
)

object HealthCheck:

  def required[F[_]](name: String, run: F[Unit]): HealthCheck[F] =
    HealthCheck(name, Importance.Required, run)

  def info[F[_]](name: String, run: F[Unit]): HealthCheck[F] =
    HealthCheck(name, Importance.Info, run)

  /** Passes only on a 2xx. A 4xx from a misconfigured host or a CDN block is an outage too. */
  def reachable[F[_]: Concurrent](client: Client[F], uri: Uri): F[Unit] =
    client.status(Request[F](Method.GET, uri)).flatMap: status =>
      Concurrent[F]
        .raiseError(new RuntimeException(s"$uri answered $status"))
        .unlessA(status.isSuccess)

  /**
   * `SELECT 1` on a session of its own. Pass an untraced single-connection resource rather than
   * the application pool, so a saturated pool is not reported as Postgres being down and the
   * probe leaves no spans.
   */
  def postgres[F[_]: Concurrent](session: Resource[F, Session[F]]): F[Unit] =
    session.use(_.unique(sql"SELECT 1".query(int4))).void
