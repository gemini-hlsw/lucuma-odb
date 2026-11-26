// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.common.config

import ciris.*

/** Reads `name` from the environment, or else from the system properties. */
def envOrProp(name: String): ConfigValue[Effect, String] =
  env(name).or(prop(name))
