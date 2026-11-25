// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service.config

import cats.implicits.*
import ciris.*
import lucuma.common.config.Environment
import lucuma.common.config.Environment.Dev
import lucuma.common.config.Environment.Local
import lucuma.common.config.Environment.Production
import lucuma.common.config.Environment.Staging
import org.http4s.Uri.Host
import org.http4s.Uri.RegName

final case class OrcidConfig(
  clientId:     String,
  clientSecret: String,
  orcidHost:    Host,
)

object OrcidConfig {

  // We can't fake this part for running locally, you really do need to have ORCID credentials. See
  // the project README for information on setting this up.

  def orcidHost(env: Environment): Host =
    env match {
      case Local   | Dev        => RegName("sandbox.orcid.org")
      case Staging | Production => RegName("orcid.org")
    }

  val config: ConfigValue[Effect, Environment => OrcidConfig] = (
    envOrProp("LUCUMA_ORCID_CLIENT_ID"),
    envOrProp("LUCUMA_ORCID_CLIENT_SECRET").redacted
  ).parMapN((id, secret) => env => apply(id, secret, orcidHost(env)))

}
