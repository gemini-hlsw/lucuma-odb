// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import ciris.ConfigValue
import ciris.Effect
import ciris.env
import ciris.prop

package object config {
  def envOrProp(name: String): ConfigValue[Effect, String] =
    env(name).or(prop(name))

}
