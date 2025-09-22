// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service.config

import cats.implicits.*
import ciris.*
import ciris.ConfigValue.configValueNonEmptyParallel

case class HoneycombConfig(
  writeKey: String,
  dataset:  String
)

object HoneycombConfig {

  val config: ConfigValue[Effect, HoneycombConfig] =
    (envOrProp("HONEYCOMB_WRITE_KEY"), envOrProp("HONEYCOMB_DATASET")).parMapN((a, b) =>
      HoneycombConfig(a, b)
    )

}
