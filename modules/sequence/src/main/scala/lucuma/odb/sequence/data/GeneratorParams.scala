// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package data

import cats.Eq
import cats.data.NonEmptyList
import cats.syntax.eq.*
import lucuma.core.model.Target
import lucuma.itc.client.SpectroscopyIntegrationTimeInput

enum GeneratorParams {

  case GmosNorthLongSlit(
    itc:  NonEmptyList[(Target.Id, SpectroscopyIntegrationTimeInput)],
    mode: gmos.longslit.Config.GmosNorth
  )

  case GmosSouthLongSlit(
    itc:  NonEmptyList[(Target.Id, SpectroscopyIntegrationTimeInput)],
    mode: gmos.longslit.Config.GmosSouth
  )

  def observingMode: ObservingMode =
    this match {
      case GmosNorthLongSlit(_, m) => m
      case GmosSouthLongSlit(_, m) => m
    }

}

object GeneratorParams {

  given Eq[GmosNorthLongSlit] =
    Eq.by { a => (
      a.itc,
      a.mode
    )}

  given Eq[GmosSouthLongSlit] =
    Eq.by { a => (
      a.itc,
      a.mode
    )}

  given Eq[GeneratorParams] =
    Eq.instance {
      case (n0@GmosNorthLongSlit(_, _), n1@GmosNorthLongSlit(_, _)) => n0 === n1
      case (s0@GmosSouthLongSlit(_, _), s1@GmosSouthLongSlit(_, _)) => s0 === s1
      case _                                                        => false
    }

}
