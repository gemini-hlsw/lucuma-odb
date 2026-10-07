// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.otel.health

import cats.effect.Concurrent
import cats.syntax.all.*
import org.http4s.Method
import org.http4s.Request
import org.http4s.Status
import org.http4s.Uri
import org.http4s.client.Client

enum Importance derives CanEqual:
  /** A failure makes the service unable to do its job, readiness reports 503. */
  case Required
  /** A failure degrades a feature but the service keeps serving, readiness stays 200. */
  case Informational

/** A named dependency probe. `run` succeeds when the dependency is reachable and raises otherwise. */
final case class HealthCheck[F[_]](
  name:       String,
  importance: Importance,
  run:        F[Unit]
)

object HealthCheck:

  def required[F[_]](name: String, run: F[Unit]): HealthCheck[F] =
    HealthCheck(name, Importance.Required, run)

  def informational[F[_]](name: String, run: F[Unit]): HealthCheck[F] =
    HealthCheck(name, Importance.Informational, run)

  /** Any answer short of a server error passes, so a peer that predates `/health` still counts. */
  def reachable[F[_]: Concurrent](client: Client[F], uri: Uri): F[Unit] =
    client.status(Request[F](Method.GET, uri)).flatMap: status =>
      Concurrent[F]
        .raiseError(new RuntimeException(s"$uri answered $status"))
        .whenA(status.responseClass == Status.ServerError)
