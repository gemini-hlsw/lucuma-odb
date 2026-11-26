// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service.config

import cats.syntax.all.*
import ciris.*
import ciris.http4s.*
import lucuma.common.config.Environment
import lucuma.common.config.envOrProp
import lucuma.otel.OtelConfig
import org.http4s.Uri

/**
 * Application configuration.
 */
final case class Config(
  environment:               Environment,
  port:                      Int,
  redisUrl:                  Option[Uri],
  odbBaseUrl:                Uri,
  odbServiceToken:           String,
  otel:                      Option[OtelConfig],
  inHeroku:                  Boolean,
  cacheTtlDays:              Int,
  maxConcurrentCalculations: Int
)

object Config:

  def config: ConfigValue[Effect, Config] =
    val environment = Environment.fromCiris("ODB_ENVIRONMENT")
    val dynoCheck   = env("DYNO").option
    (environment,
     envOrProp("ITC_PORT")
       .or(envOrProp("PORT"))
       .or(ConfigValue.default("6060"))
       .as[Int],
     redisUrlConfig(dynoCheck),
     envOrProp("ODB_BASE_URL").as[Uri],
     envOrProp("ODB_SERVICE_JWT").redacted,
     OtelConfig.fromEnv("ODB_OTEL", environment.map(_.name)),
     dynoCheck.map(_.isDefined),
     envOrProp("ITC_CACHE_TTL_DAYS").as[Int].default(7),
     envOrProp("ITC_MAX_CONCURRENT_CALCULATIONS").as[Int].default(4)
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
