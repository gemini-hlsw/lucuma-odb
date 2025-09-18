// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service.config

import cats.syntax.all.*
import ciris.*
import org.http4s.Uri

import scala.concurrent.duration.*

case class GraphiteConfig(
  prefix: String,        // this allows a way to identify the host. We should define a naming convention
  url:    Uri,
  apiKey: Option[String] // hosted graphithe requires a key
):
  val fullPrefix = apiKey.fold(prefix)(k => s"$k.${prefix}")

case class MetricsConfig(
  graphite:  Option[GraphiteConfig],
  frequency: FiniteDuration
)

object MetricsConfig:

  given ConfigDecoder[String, Uri] =
    ConfigDecoder[String].mapOption("URI"): s =>
      Uri.fromString(s).toOption

  val config: ConfigValue[Effect, MetricsConfig] =
    (
      envOrProp("METRICS_PREFIX").option,
      envOrProp("METRICS_REPORTING_FREQUENCY").as[Int].default(60).map(_.seconds),
      envOrProp("GRAPHITE_URL").as[Uri].option,
      envOrProp("GRAPHITE_API_KEY").option,
      env("DYNO").option
    ).parMapN: (prefix, frequency, graphiteUrl, apiKey, dyno) =>
      val graphite = (prefix, graphiteUrl).mapN { (basePrefix, url) =>
        val finalPrefix = dyno.fold(basePrefix)(d => s"$basePrefix.$d")
        GraphiteConfig(finalPrefix, url, apiKey)
      }

      MetricsConfig(graphite, frequency)
