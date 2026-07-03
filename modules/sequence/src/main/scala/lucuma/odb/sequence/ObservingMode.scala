// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.Eq
import cats.syntax.eq.*
import cats.syntax.option.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.odb.sequence.exchange.Config as Exchange
import lucuma.odb.sequence.flamingos2.imaging.Config as Flamingos2Imaging
import lucuma.odb.sequence.flamingos2.longslit.Config as Flamingos2LongSlit
import lucuma.odb.sequence.ghost.ifu.Config as GhostIfu
import lucuma.odb.sequence.gmos.imaging.Config.GmosNorth as GmosNorthImaging
import lucuma.odb.sequence.gmos.imaging.Config.GmosSouth as GmosSouthImaging
import lucuma.odb.sequence.gmos.longslit.Config.GmosNorth as GmosNorthLongSlit
import lucuma.odb.sequence.gmos.longslit.Config.GmosSouth as GmosSouthLongSlit
import lucuma.odb.sequence.gnirs.spectroscopy.Config as GnirsSpectroscopy
import lucuma.odb.sequence.igrins2.longslit.Config as Igrins2LongSlit
import lucuma.odb.sequence.syntax.all.hashBytes
import lucuma.odb.sequence.util.HashBytes
import lucuma.odb.sequence.visitor.Config as Visitor

/**
 * All observing mode options.
 */
type ObservingMode =
  Exchange           |
  Flamingos2Imaging  |
  Flamingos2LongSlit |
  GhostIfu           |
  GmosNorthImaging   |
  GmosNorthLongSlit  |
  GmosSouthImaging   |
  GmosSouthLongSlit  |
  GnirsSpectroscopy  |
  Igrins2LongSlit    |
  Visitor

object ObservingMode:

  val ExchangeName: String           = "Exchange"
  val Flamingos2ImagingName: String  = "Flamingos 2 Imaging"
  val Flamingos2LongSlitName: String = "Flamingos 2 Long Slit"
  val GhostIfuName: String           = "GHOST IFU"
  val GmosNorthImagingName: String   = "GMOS North Imaging"
  val GmosNorthLongSlitName: String  = "GMOS North Long Slit"
  val GmosSouthImagingName: String   = "GMOS South Imaging"
  val GmosSouthLongSlitName: String  = "GMOS South Long Slit"
  val GnirsSpectroscopyName: String  = "GNIRS Spectroscopy"
  val Igrins2LongSlitName: String    = "IGRINS-2 Long Slit"
  val VisitorName: String            = "Visitor"

  object Instances:
    given Eq[ObservingMode] =
      Eq.instance:
        case (a: Exchange,           b: Exchange)           => a === b
        case (a: Flamingos2LongSlit, b: Flamingos2LongSlit) => a === b
        case (a: Flamingos2Imaging,  b: Flamingos2Imaging)  => a === b
        case (a: GhostIfu,           b: GhostIfu)           => a === b
        case (a: GmosNorthLongSlit,  b: GmosNorthLongSlit)  => a === b
        case (a: GmosSouthLongSlit,  b: GmosSouthLongSlit)  => a === b
        case (a: GmosNorthImaging,   b: GmosNorthImaging)   => a === b
        case (a: GmosSouthImaging,   b: GmosSouthImaging)   => a === b
        case (a: GnirsSpectroscopy,  b: GnirsSpectroscopy)  => a === b
        case (a: Igrins2LongSlit,    b: Igrins2LongSlit)    => a === b
        case (a: Visitor,            b: Visitor)            => a === b
        case _                                               => false

    given HashBytes[ObservingMode] =
      case exc: Exchange           => exc.hashBytes
      case f2:  Flamingos2LongSlit => f2.hashBytes
      case f2i: Flamingos2Imaging  => f2i.hashBytes
      case ghs: GhostIfu           => ghs.hashBytes
      case gnl: GmosNorthLongSlit  => gnl.hashBytes
      case gsl: GmosSouthLongSlit  => gsl.hashBytes
      case gni: GmosNorthImaging   => gni.hashBytes
      case gsi: GmosSouthImaging   => gsi.hashBytes
      case gns: GnirsSpectroscopy  => gns.hashBytes
      case ig2: Igrins2LongSlit    => ig2.hashBytes
      case vis: Visitor            => vis.hashBytes

  object Syntax:
    extension (m: ObservingMode)
      // Exchange observations are not Gemini instruments and have no `Instrument`.
      def instrument: Option[Instrument] =
        m match
          case _: Exchange           => none
          case _: Flamingos2Imaging  => Instrument.Flamingos2.some
          case _: Flamingos2LongSlit => Instrument.Flamingos2.some
          case _: GhostIfu           => Instrument.Ghost.some
          case _: GmosNorthImaging   => Instrument.GmosNorth.some
          case _: GmosNorthLongSlit  => Instrument.GmosNorth.some
          case _: GmosSouthImaging   => Instrument.GmosSouth.some
          case _: GmosSouthLongSlit  => Instrument.GmosSouth.some
          case _: GnirsSpectroscopy  => Instrument.Gnirs.some
          case _: Igrins2LongSlit    => Instrument.Igrins2.some
          case v: Visitor            => v.mode.instrument.some

      def name: String =
        m match
          case _: Exchange           => ExchangeName
          case _: Flamingos2Imaging  => Flamingos2ImagingName
          case _: Flamingos2LongSlit => Flamingos2LongSlitName
          case _: GhostIfu           => GhostIfuName
          case _: GmosNorthImaging   => GmosNorthImagingName
          case _: GmosNorthLongSlit  => GmosNorthLongSlitName
          case _: GmosSouthImaging   => GmosSouthImagingName
          case _: GmosSouthLongSlit  => GmosSouthLongSlitName
          case _: GnirsSpectroscopy  => GnirsSpectroscopyName
          case _: Igrins2LongSlit    => Igrins2LongSlitName
          case _: Visitor            => VisitorName

      def modeType: ObservingModeType =
        m match
          case e: Exchange           => e.mode
          case _: Flamingos2Imaging  => ObservingModeType.Flamingos2Imaging
          case _: Flamingos2LongSlit => ObservingModeType.Flamingos2LongSlit
          case _: GhostIfu           => ObservingModeType.GhostIfu
          case _: GmosNorthImaging   => ObservingModeType.GmosNorthImaging
          case _: GmosNorthLongSlit  => ObservingModeType.GmosNorthLongSlit
          case _: GmosSouthImaging   => ObservingModeType.GmosSouthImaging
          case _: GmosSouthLongSlit  => ObservingModeType.GmosSouthLongSlit
          case a: GnirsSpectroscopy  =>
            a.fpu match
              case _: GnirsFpu.Spectroscopy.Slit => ObservingModeType.GnirsLongSlit
              case _: GnirsFpu.Spectroscopy.Ifu  => ObservingModeType.GnirsIfu
          case _: Igrins2LongSlit    => ObservingModeType.Igrins2LongSlit
          case v: Visitor            => v.mode

      def asExchange: Option[Exchange] =
        m match
          case a: Exchange => a.some
          case _           => none

      def asFlamingos2Imaging: Option[Flamingos2Imaging] =
        m match
          case a: Flamingos2Imaging => a.some
          case _                    => none

      def asFlamingos2LongSlit: Option[Flamingos2LongSlit] =
        m match
          case a: Flamingos2LongSlit => a.some
          case _                     => none

      def asGhostIfu: Option[GhostIfu] =
        m match
          case a: GhostIfu => a.some
          case _           => none

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

      def asGnirsSpectroscopy: Option[GnirsSpectroscopy] =
        m match
          case a: GnirsSpectroscopy => a.some
          case _                    => none

      def asIgrins2LongSlit: Option[Igrins2LongSlit] =
        m match
          case a: Igrins2LongSlit => a.some
          case _                  => none

      def asVisitor: Option[Visitor] =
        m match
          case a: Visitor => a.some
          case _          => none