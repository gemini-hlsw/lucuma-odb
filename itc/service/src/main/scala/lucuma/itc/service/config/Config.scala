// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service.config

import cats.syntax.all.*
import ciris.*
import org.http4s.Uri

/**
 * Application configuration.
 */
final case class Config(
  environment:     ExecutionEnvironment,
  port:            Int,
  redisUrl:        Option[Uri],
  odbBaseUrl:      Uri,
  odbServiceToken: String,
  honeycomb:       Option[HoneycombConfig],
  inHeroku:        Boolean,
  metrics:         MetricsConfig,
  cacheTtlDays:    Int
)

object Config:

  given ConfigDecoder[String, Uri] =
    ConfigDecoder[String].mapOption("URI") { s =>
      Uri.fromString(s).toOption
    }

  def config: ConfigValue[Effect, Config] =
    val dynoCheck = env("DYNO").option
    (envOrProp("LUCUMA_SSO_ENVIRONMENT")
       .as[ExecutionEnvironment]
       .default(ExecutionEnvironment.Local),
     envOrProp("ITC_PORT")
       .or(envOrProp("PORT"))
       .or(ConfigValue.default("6060"))
       .as[Int],
     redisUrlConfig(dynoCheck),
     envOrProp("ODB_BASE_URL").as[Uri],
     envOrProp("ODB_SERVICE_JWT"),
     HoneycombConfig.config.option,
     dynoCheck.map(_.isDefined),
     MetricsConfig.config,
     envOrProp("ITC_CACHE_TTL_DAYS").as[Int].default(7)
    ).parMapN(Config.apply)

  private def redisUrlConfig(
    dynoCheck: ConfigValue[Effect, Option[String]]
  ): ConfigValue[Effect, Option[Uri]] =
    val redisUrl = envOrProp("REDISCLOUD_URL")
      .or(envOrProp("REDIS_URL"))
      .as[Uri]

    dynoCheck.flatMap:
      case Some(_) => redisUrl.map(Some(_))
      case None    => redisUrl.option
