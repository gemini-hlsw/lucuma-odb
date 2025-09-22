// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service.config

import cats.implicits.*
import ciris.*

enum ExecutionEnvironment:
  case Local, Review, Staging, Production

given ConfigDecoder[String, ExecutionEnvironment] =
  ConfigDecoder[String].map(_.toLowerCase).collect("Environment") {
    case "local"      => ExecutionEnvironment.Local
    case "review"     => ExecutionEnvironment.Review
    case "staging"    => ExecutionEnvironment.Staging
    case "production" => ExecutionEnvironment.Production
  }
