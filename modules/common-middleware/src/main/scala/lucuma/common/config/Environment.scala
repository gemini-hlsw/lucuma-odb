// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.common.config

import cats.Eq
import cats.syntax.all.*
import ciris.*

/** The execution environment of a service. */
enum Environment:
  case Local, Development, Staging, Production

  /** The canonical lowercase name, for example for telemetry. */
  def name: String = toString.toLowerCase

  /** True only for `Local`, the one environment that relaxes security settings. */
  def isLocal: Boolean = this == Local

object Environment:

  given Eq[Environment] = Eq.fromUniversalEquals

  given ConfigDecoder[String, Environment] =
    ConfigDecoder[String].map(_.trim.toLowerCase).collect("Environment") {
      case "local"               => Local
      case "dev" | "development" => Development
      case "staging"             => Staging
      case "production"          => Production
    }

  /**
   * Loads the environment from the variable (or system property) `key`.
   *
   * There is deliberately no default: `Local` relaxes security settings (for example cookies and
   * header logging), so a deployment that forgets the variable must fail to start rather than
   * silently run with local settings. For the same reason `Local` is refused when `DYNO` is set,
   * because Heroku sets `DYNO` in every dyno.
   */
  def fromCiris(key: String): ConfigValue[Effect, Environment] =
    load(key, name => env(name).or(prop(name)))

  /** As `fromCiris`, with the lookup supplied, so tests do not depend on the process environment. */
  private[config] def load(
    key:    String,
    lookup: String => ConfigValue[Effect, String]
  ): ConfigValue[Effect, Environment] =
    (lookup(key).as[Environment], lookup("DYNO").option).parFlatMapN {
      case (Local, Some(dyno)) =>
        ConfigValue.failed(ConfigError(s"$key=local is not allowed on Heroku (DYNO is set to $dyno)"))
      case (env, _)            =>
        ConfigValue.loaded(ConfigKey(key), env)
    }
