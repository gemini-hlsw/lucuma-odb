// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package data

import cats.Eq
import cats.data.NonEmptyList
import cats.syntax.eq.*
import eu.timepit.refined.cats.*
import lucuma.core.model.Target
import lucuma.itc.client.SpectroscopyModeInput

enum GeneratorParams {

  case GmosNorthLongSlit(
    itc:    NonEmptyList[(Target.Id, SpectroscopyModeInput)],
    config: gmos.longslit.Config.GmosNorth
  )

  case GmosSouthLongSlit(
    itc:    NonEmptyList[(Target.Id, SpectroscopyModeInput)],
    config: gmos.longslit.Config.GmosSouth
  )

}

object GeneratorParams {

  // TODO: needs to be put into itc client
  given Eq[SpectroscopyModeInput] =
    Eq.by { a => (
      a.wavelength,
      a.signalToNoise,
      a.signalToNoiseAt,
      a.sourceProfile,
      a.band,
      a.radialVelocity,
      a.constraints,
      a.mode
    )}

  given Eq[GmosNorthLongSlit] =
    Eq.by { a => (
      a.itc,
      a.config
    )}

  given Eq[GmosSouthLongSlit] =
    Eq.by { a => (
      a.itc,
      a.config
    )}

  given Eq[GeneratorParams] =
    Eq.instance {
      case (n0@GmosNorthLongSlit(_, _), n1@GmosNorthLongSlit(_, _)) => n0 === n1
      case (s0@GmosSouthLongSlit(_, _), s1@GmosSouthLongSlit(_, _)) => s0 === s1
      case _                                                        => false
    }

}
