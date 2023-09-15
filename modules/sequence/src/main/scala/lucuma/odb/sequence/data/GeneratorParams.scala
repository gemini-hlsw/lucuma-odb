// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package data

import cats.Eq
import cats.data.NonEmptyList
import cats.syntax.eq.*
import lucuma.core.model.Target
import lucuma.itc.client.ImagingIntegrationTimeInput
import lucuma.itc.client.SpectroscopyIntegrationTimeInput
import lucuma.odb.sequence.syntax.all.*
import lucuma.odb.sequence.util.HashBytes

enum GeneratorParams {

  case GmosNorthLongSlit(
    itc:  NonEmptyList[(Target.Id, (ImagingIntegrationTimeInput, SpectroscopyIntegrationTimeInput))],
    mode: gmos.longslit.Config.GmosNorth
  )

  case GmosSouthLongSlit(
    itc:  NonEmptyList[(Target.Id, (ImagingIntegrationTimeInput, SpectroscopyIntegrationTimeInput))],
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

  private def itcBytes(
    itc: NonEmptyList[(Target.Id, (ImagingIntegrationTimeInput, SpectroscopyIntegrationTimeInput))]
  ): Array[Byte] = {
    val bld = scala.collection.mutable.ArrayBuilder.make[Byte]

    itc.toList.foreach { case (tid, (imaging, spectroscopy)) =>
      bld.addAll(tid.hashBytes)
      bld.addAll(imaging.hashBytes)
      bld.addAll(spectroscopy.hashBytes)
    }

    bld.result()
  }

  given HashBytes[GeneratorParams] with {
    def hashBytes(a: GeneratorParams): Array[Byte] =
      a match {
        case GmosNorthLongSlit(itc, mode) => Array.concat(itcBytes(itc), mode.hashBytes)
        case GmosSouthLongSlit(itc, mode) => Array.concat(itcBytes(itc), mode.hashBytes)
      }
  }

}
