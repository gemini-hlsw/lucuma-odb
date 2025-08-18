// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service.config

import cats.implicits.*
import ciris.*

sealed trait Environment
object Environment {

  case object Local      extends Environment
  case object Review     extends Environment
  case object Staging    extends Environment
  case object Production extends Environment

  implicit val ConfigDecoderEnvironment: ConfigDecoder[String, Environment] =
    ConfigDecoder[String].map(_.toLowerCase).collect("Environment") {
      case "local" => Local
      case "review"   => Review
      case "staging"  => Staging
      case "production" => Production
    }

}
