// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.model.config

import cats.implicits.*
import ciris.*

enum ExecutionEnvironment:
  case Local, Development, Staging, Production

  def stringValue: String =
    this match
      case Local       => "local"
      case Development => "development"
      case Staging     => "staging"
      case Production  => "production"

given ConfigDecoder[String, ExecutionEnvironment] =
  ConfigDecoder[String].map(_.toLowerCase).collect("Environment") {
    case "local"       => ExecutionEnvironment.Local
    case "development" => ExecutionEnvironment.Development
    case "staging"     => ExecutionEnvironment.Staging
    case "production"  => ExecutionEnvironment.Production
  }
