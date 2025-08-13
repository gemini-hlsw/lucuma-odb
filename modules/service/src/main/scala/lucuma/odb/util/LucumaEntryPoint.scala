// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.util

import cats.effect.*
import cats.syntax.all.*
import lucuma.odb.Config
import natchez.EntryPoint
import natchez.honeycomb.Honeycomb
import natchez.noop.NoopEntrypoint
import org.typelevel.log4cats.Logger

object LucumaEntryPoint:
  def tracingBackend(config: Config) = config.honeycomb match
    case Some(_) => "Honeycomb"
    case None    => "No-op (silent)"

  def entryPointResource[F[_]: Sync: Logger](serviceName: String, config: Config): Resource[F, EntryPoint[F]] =
    config.honeycomb match
      case Some(honeycombConfig) =>
        Resource.eval(Logger[F].info("Initializing Honeycomb tracing backend")) *>
          Honeycomb.entryPoint(serviceName) { cb =>
            Sync[F].delay {
              cb.setWriteKey(honeycombConfig.writeKey)
              cb.setDataset(honeycombConfig.dataset)
              cb.build()
            }
          }
      case None =>
        Resource.eval(Logger[F].info("No Honeycomb configuration")) *>
          Resource.pure(NoopEntrypoint())
