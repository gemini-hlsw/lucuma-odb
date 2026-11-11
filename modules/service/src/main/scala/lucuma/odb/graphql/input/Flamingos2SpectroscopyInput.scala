// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.eq.*
import grackle.Result
import lucuma.core.enums.Flamingos2AcquisitionType
import lucuma.core.math.Offset
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.odb.data.Nullable

/**
 * Input validation shared by the Flamingos 2 spectroscopy modes.
 */
object Flamingos2SpectroscopyInput:

  // Flamingos2's ABBA science pattern requires exactly 4 telescope configs.
  def validateTelescopeConfigs(tc: SlitTelescopeConfigs): Result[SlitTelescopeConfigs] =
    val n = tc.telescopeConfigs.size
    if n === 4 then Result(tc)
    else Result.failure(s"Flamingos2 must have exactly 4 offsets, but $n were provided.")

  // The DB enforces this on the row as well, so it must hold within a single input.
  def validateSkyOffset(
    explicitAcquisitionType: Nullable[Flamingos2AcquisitionType],
    skyOffset:               Option[Offset]
  ): Result[Unit] =
    val explicitlyFaint: Boolean =
      explicitAcquisitionType.toOption.contains(Flamingos2AcquisitionType.Faint)
    (skyOffset.isDefined, explicitlyFaint) match
      case (true, false) => Result.failure("'skyOffset' is only valid when 'explicitAcquisitionType' is FAINT.")
      case (false, true) => Result.failure("'explicitAcquisitionType' FAINT requires a 'skyOffset'.")
      case _             => Result.unit
