// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service.metrics

import cats.effect.*
import cats.syntax.all.*
import com.codahale.metrics.*
import com.codahale.metrics.graphite.*
import com.codahale.metrics.jvm.*
import lucuma.itc.service.config.GraphiteConfig
import lucuma.itc.service.config.MetricsConfig
import org.typelevel.log4cats.Logger

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

object MetricsService:
  def resource[F[_]: Async: Logger](config: MetricsConfig): Resource[F, Unit] =
    // We may add more metrics in the future
    JvmMetricsService.resource(config)

object JvmMetricsService:

  def resource[F[_]: Async: Logger](config: MetricsConfig): Resource[F, Unit] =
    config.graphite match
      case Some(graphite) =>
        Resource.make {
          for {
            reporter <- startReporter(graphite, config.frequency)
            _        <-
              Logger[F].info(
                s"Start JVM metrics reporting to ${graphite.url} on '${graphite.prefix}'"
              )
          } yield reporter
        } { reporter =>
          Sync[F].delay {
            reporter.stop()
            ()
          }
        }.void
      case _              =>
        Resource.eval(Logger[F].info("JVM metrics disabled")).void

  private def startReporter[F[_]: Sync](config: GraphiteConfig, interval: FiniteDuration) =
    Sync[F].delay {

      val registry = new MetricRegistry()

      // JVM metrics with prefixes
      registry.registerAll("jvm.memory", new MemoryUsageGaugeSet())
      registry.registerAll("jvm.gc", new GarbageCollectorMetricSet())
      registry.registerAll("jvm.threads", new ThreadStatesGaugeSet())
      registry.registerAll("jvm.classloading", new ClassLoadingGaugeSet())

      val transport = config.url.scheme.map(_.value).getOrElse("tcp")
      val host      = config.url.host.map(_.value).getOrElse("localhost")
      val port      = config.url.port.getOrElse(2003)

      val graphiteSender = transport match {
        case "udp" => new GraphiteUDP(host, port)
        case _     => new Graphite(host, port) // Default TCP
      }

      val reporter = GraphiteReporter
        .forRegistry(registry)
        .prefixedWith(config.fullPrefix)
        .convertRatesTo(TimeUnit.SECONDS)
        .convertDurationsTo(TimeUnit.MILLISECONDS)
        .filter(MetricFilter.ALL)
        .build(graphiteSender)

      reporter.start(interval.toSeconds, TimeUnit.SECONDS)
      reporter
    }
