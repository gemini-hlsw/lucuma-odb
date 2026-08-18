// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.eq.*
import grackle.Result
import lucuma.core.model.SlitTelescopeConfigs

/**
 * Input validation shared by the Flamingos 2 spectroscopy modes.
 */
object Flamingos2SpectroscopyInput:

  // Flamingos2's ABBA science pattern requires exactly 4 telescope configs.
  def validateTelescopeConfigs(tc: SlitTelescopeConfigs): Result[SlitTelescopeConfigs] =
    val n = tc.telescopeConfigs.size
    if n === 4 then Result(tc)
    else Result.failure(s"Flamingos2 must have exactly 4 offsets, but $n were provided.")
