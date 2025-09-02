// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.Eq
import cats.syntax.eq.*
import lucuma.core.enums.ObservingModeType
import lucuma.odb.sequence.flamingos2.longslit.Config as Flamingos2Config
import lucuma.odb.sequence.gmos.imaging.Config.GmosNorth as GmosNorthImaging
import lucuma.odb.sequence.gmos.imaging.Config.GmosSouth as GmosSouthImaging
import lucuma.odb.sequence.gmos.longslit.Config.GmosNorth as GmosNorthLongSlit
import lucuma.odb.sequence.gmos.longslit.Config.GmosSouth as GmosSouthLongSlit
import lucuma.odb.sequence.util.HashBytes

/**
 * All observing mode options.
 */
type ObservingMode = GmosNorthLongSlit | GmosSouthLongSlit | GmosNorthImaging | GmosSouthImaging | Flamingos2Config

given Eq[ObservingMode] =
  Eq.instance:
    case (a: Flamingos2Config,    b: Flamingos2Config)    => a === b
    case (a: GmosNorthLongSlit,   b: GmosNorthLongSlit)   => a === b
    case (a: GmosSouthLongSlit,   b: GmosSouthLongSlit)   => a === b
    case (a: GmosNorthImaging,    b: GmosNorthImaging)    => a === b
    case (a: GmosSouthImaging,    b: GmosSouthImaging)    => a === b
    case _                                                => false

extension (self: ObservingMode)
  def observingModeType: ObservingModeType =
    self match
      case _: Flamingos2Config    => ObservingModeType.Flamingos2LongSlit
      case _: GmosNorthLongSlit   => ObservingModeType.GmosNorthLongSlit
      case _: GmosSouthLongSlit   => ObservingModeType.GmosSouthLongSlit
      case _: GmosNorthImaging    => ObservingModeType.GmosNorthImaging
      case _: GmosSouthImaging    => ObservingModeType.GmosSouthImaging

object ObservingMode:

  given HashBytes[ObservingMode] =
    case f2:  Flamingos2Config    => f2.hashBytes
    case gnl: GmosNorthLongSlit   => gnl.hashBytes
    case gsl: GmosSouthLongSlit   => gsl.hashBytes
    case gni: GmosNorthImaging    => gni.hashBytes
    case gsi: GmosSouthImaging    => gsi.hashBytes

