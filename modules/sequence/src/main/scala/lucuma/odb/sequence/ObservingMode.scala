// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.Eq
import cats.syntax.eq.*
import cats.syntax.option.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservingModeType
import lucuma.odb.sequence.flamingos2.longslit.Config as Flamingos2LongSlit
import lucuma.odb.sequence.gmos.imaging.Config.GmosNorth as GmosNorthImaging
import lucuma.odb.sequence.gmos.imaging.Config.GmosSouth as GmosSouthImaging
import lucuma.odb.sequence.gmos.longslit.Config.GmosNorth as GmosNorthLongSlit
import lucuma.odb.sequence.gmos.longslit.Config.GmosSouth as GmosSouthLongSlit
import lucuma.odb.sequence.util.HashBytes

/**
 * All observing mode options.
 */
type ObservingMode =
  Flamingos2LongSlit |
  GmosNorthImaging   |
  GmosNorthLongSlit  |
  GmosSouthImaging   |
  GmosSouthLongSlit

object ObservingMode:

  val Flamingos2LongSlitName: String = "Flamingos 2 Long Slit"
  val GmosNorthImagingName: String   = "GMOS North Imaging"
  val GmosNorthLongSlitName: String  = "GMOS North Long Slit"
  val GmosSouthImagingName: String   = "GMOS South Imaging"
  val GmosSouthLongSlitName: String  = "GMOS South Long Slit"

  object Instances:
    given Eq[ObservingMode] =
      Eq.instance:
        case (a: Flamingos2LongSlit, b: Flamingos2LongSlit) => a === b
        case (a: GmosNorthLongSlit,  b: GmosNorthLongSlit)  => a === b
        case (a: GmosSouthLongSlit,  b: GmosSouthLongSlit)  => a === b
        case (a: GmosNorthImaging,   b: GmosNorthImaging)   => a === b
        case (a: GmosSouthImaging,   b: GmosSouthImaging)   => a === b
        case _                                              => false

    given HashBytes[ObservingMode] =
      case f2:  Flamingos2LongSlit => f2.hashBytes
      case gnl: GmosNorthLongSlit  => gnl.hashBytes
      case gsl: GmosSouthLongSlit  => gsl.hashBytes
      case gni: GmosNorthImaging   => gni.hashBytes
      case gsi: GmosSouthImaging   => gsi.hashBytes

  object Syntax:
    extension (m: ObservingMode)
      def instrument: Instrument =
        m match
          case _: Flamingos2LongSlit => Instrument.Flamingos2
          case _: GmosNorthImaging   => Instrument.GmosNorth
          case _: GmosNorthLongSlit  => Instrument.GmosNorth
          case _: GmosSouthImaging   => Instrument.GmosSouth
          case _: GmosSouthLongSlit  => Instrument.GmosSouth

      def name: String =
        m match
          case _: Flamingos2LongSlit => Flamingos2LongSlitName
          case _: GmosNorthImaging   => GmosNorthImagingName
          case _: GmosNorthLongSlit  => GmosNorthLongSlitName
          case _: GmosSouthImaging   => GmosSouthImagingName
          case _: GmosSouthLongSlit  => GmosSouthLongSlitName

      def modeType: ObservingModeType =
        m match
          case _: Flamingos2LongSlit => ObservingModeType.Flamingos2LongSlit
          case _: GmosNorthImaging   => ObservingModeType.GmosNorthImaging
          case _: GmosNorthLongSlit  => ObservingModeType.GmosNorthLongSlit
          case _: GmosSouthImaging   => ObservingModeType.GmosSouthImaging
          case _: GmosSouthLongSlit  => ObservingModeType.GmosSouthLongSlit

      def asFlamingos2LongSlit: Option[Flamingos2LongSlit] =
        m match
          case a: Flamingos2LongSlit => a.some
          case _                     => none

      def asGmosNorthImaging: Option[GmosNorthImaging] =
        m match
          case a: GmosNorthImaging => a.some
          case _                   => none

      def asGmosNorthLongSlit: Option[GmosNorthLongSlit] =
        m match
          case a: GmosNorthLongSlit => a.some
          case _                    => none

      def asGmosSouthImaging: Option[GmosSouthImaging] =
        m match
          case a: GmosSouthImaging => a.some
          case _                   => none

      def asGmosSouthLongSlit: Option[GmosSouthLongSlit] =
        m match
          case a: GmosSouthLongSlit => a.some
          case _                    => none