// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import cats.derived.*
import cats.syntax.eq.*
import cats.syntax.functor.*
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.PortDisposition
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.itc.client.json.encoders.given
import lucuma.itc.client.json.syntax.*
import lucuma.odb.json.gmos.given
import lucuma.odb.json.wavelength.transport.given
import monocle.Prism
import monocle.macros.GenPrism

sealed trait InstrumentMode:
  // This will return "an" exposure time mode. For most instruments this is "the" exposure time mode,
  // but for some (e.g. GHOST) there are multiple. In the case of GHOST, the ITC will ignore the
  // "top level" exposure time mode and use the one in the spectroscopy mode, but we still need
  // to have one to satisfy the legacy interface.
  // This is also used in PerProgramPerConfigCalibrationsService. That might need to be updated
  // if it needs to create calibrations for GHOST.
  def exposureTimeMode: ExposureTimeMode
  def displayName: String

object InstrumentMode {

  case class GmosNorthSpectroscopy(
    exposureTimeMode:  ExposureTimeMode,
    centralWavelength: Wavelength,
    grating:           GmosNorthGrating,
    filter:            Option[GmosNorthFilter],
    fpu:               GmosFpu.North,
    ccdMode:           Option[GmosCcdMode],
    roi:               Option[GmosRoi],
    port:              PortDisposition = PortDisposition.Side
  ) extends InstrumentMode derives Eq:
    override def displayName: String =
      "GMOS North Spectroscopy"

  object GmosNorthSpectroscopy {

    given Encoder[GmosNorthSpectroscopy] = a =>
      Json.fromFields(
        List(
          "exposureTimeMode"  -> a.exposureTimeMode.asJson,
          "centralWavelength" -> a.centralWavelength.asJson,
          "grating"           -> a.grating.asScreamingJson,
          "fpu"               -> a.fpu.asJson,
          "ccdMode"           -> a.ccdMode.asJson,
          "roi"               -> a.roi.asJson,
          "port"              -> a.port.asScreamingJson
        ) ++ a.filter.map(_.asScreamingJson).tupleLeft("filter").toList
      )

  }

  case class GmosSouthSpectroscopy(
    exposureTimeMode:  ExposureTimeMode,
    centralWavelength: Wavelength,
    grating:           GmosSouthGrating,
    filter:            Option[GmosSouthFilter],
    fpu:               GmosFpu.South,
    ccdMode:           Option[GmosCcdMode],
    roi:               Option[GmosRoi],
    port:              PortDisposition = PortDisposition.Side
  ) extends InstrumentMode derives Eq:
    override def displayName: String =
      "GMOS South Spectroscopy"

  object GmosSouthSpectroscopy:

    given Encoder[GmosSouthSpectroscopy] = a =>
      Json.fromFields(
        List(
          "exposureTimeMode"  -> a.exposureTimeMode.asJson,
          "centralWavelength" -> a.centralWavelength.asJson,
          "grating"           -> a.grating.asScreamingJson,
          "fpu"               -> a.fpu.asJson,
          "ccdMode"           -> a.ccdMode.asJson,
          "roi"               -> a.roi.asJson,
          "port"              -> a.port.asScreamingJson
        ) ++ a.filter.map(_.asScreamingJson).tupleLeft("filter").toList
      )

  case class Flamingos2Spectroscopy(
    exposureTimeMode: ExposureTimeMode,
    disperser:        Flamingos2Disperser,
    filter:           Flamingos2Filter,
    fpu:              Flamingos2Fpu,
    port:             PortDisposition = PortDisposition.Side
  ) extends InstrumentMode derives Eq:
    override def displayName: String =
      "Flamingos 2 Spectroscopy"

  object Flamingos2Spectroscopy:

    given Encoder[Flamingos2Spectroscopy] = a =>
      Json.fromFields(
        List(
          "exposureTimeMode" -> a.exposureTimeMode.asJson,
          "disperser"        -> a.disperser.asScreamingJson,
          "fpu"              -> a.fpu.asJson,
          "filter"           -> a.filter.asJson,
          "port"             -> a.port.asScreamingJson
        )
      )

  case class GmosNorthImaging(
    exposureTimeMode: ExposureTimeMode,
    filter:           GmosNorthFilter,
    ccdMode:          Option[GmosCcdMode],
    port:             PortDisposition = PortDisposition.Side
  ) extends InstrumentMode derives Eq:
    override def displayName: String =
      "GMOS North Imaging"

  object GmosNorthImaging:

    given Encoder[GmosNorthImaging] = a =>
      Json.obj(
        "exposureTimeMode" -> a.exposureTimeMode.asJson,
        "filter"           -> a.filter.asScreamingJson,
        "ccdMode"          -> a.ccdMode.asJson,
        "port"             -> a.port.asScreamingJson
      )

  case class GmosSouthImaging(
    exposureTimeMode: ExposureTimeMode,
    filter:           GmosSouthFilter,
    ccdMode:          Option[GmosCcdMode],
    port:             PortDisposition = PortDisposition.Side
  ) extends InstrumentMode derives Eq:
    override def displayName: String =
      "GMOS South Imaging"

  object GmosSouthImaging:

    given Encoder[GmosSouthImaging] = a =>
      Json.obj(
        "exposureTimeMode" -> a.exposureTimeMode.asJson,
        "filter"           -> a.filter.asScreamingJson,
        "ccdMode"          -> a.ccdMode.asJson,
        "port"             -> a.port.asScreamingJson
      )

  case class Flamingos2Imaging(
    exposureTimeMode: ExposureTimeMode,
    filter:           Flamingos2Filter,
    port:             PortDisposition = PortDisposition.Side
  ) extends InstrumentMode derives Eq:
    override def displayName: String =
      "Flamingos 2 Imaging"

  object Flamingos2Imaging:

    given Encoder[Flamingos2Imaging] = a =>
      Json.fromFields(
        List(
          "exposureTimeMode" -> a.exposureTimeMode.asJson,
          "filter"           -> a.filter.asJson,
          "port"             -> a.port.asScreamingJson
        )
      )

  val gmosNorthSpectroscopy: Prism[InstrumentMode, GmosNorthSpectroscopy] =
    GenPrism[InstrumentMode, GmosNorthSpectroscopy]

  val gmosSouthSpectroscopy: Prism[InstrumentMode, GmosSouthSpectroscopy] =
    GenPrism[InstrumentMode, GmosSouthSpectroscopy]

  val flamingos2Spectroscopy: Prism[InstrumentMode, Flamingos2Spectroscopy] =
    GenPrism[InstrumentMode, Flamingos2Spectroscopy]

  val gmosNorthImaging: Prism[InstrumentMode, GmosNorthImaging] =
    GenPrism[InstrumentMode, GmosNorthImaging]

  val gmosSouthImaging: Prism[InstrumentMode, GmosSouthImaging] =
    GenPrism[InstrumentMode, GmosSouthImaging]

  val flamingos2Imaging: Prism[InstrumentMode, Flamingos2Imaging] =
    GenPrism[InstrumentMode, Flamingos2Imaging]

  case class Igrins2Spectroscopy(
    exposureTimeMode: ExposureTimeMode,
    port:             PortDisposition = PortDisposition.Bottom
  ) extends InstrumentMode derives Eq:
    override def displayName: String =
      "IGRINS2 Spectroscopy"

  object Igrins2Spectroscopy:
    given Encoder[Igrins2Spectroscopy] = a =>
      Json.obj(
        "exposureTimeMode" -> a.exposureTimeMode.asJson,
        "port"             -> a.port.asScreamingJson
      )

  val igrins2Spectroscopy: Prism[InstrumentMode, Igrins2Spectroscopy] =
    GenPrism[InstrumentMode, Igrins2Spectroscopy]

  given Encoder[InstrumentMode] = a =>
    a match
      case a @ GmosNorthSpectroscopy(_, _, _, _, _, _, _, _) =>
        Json.obj("gmosNSpectroscopy" -> a.asJson)
      case a @ GmosSouthSpectroscopy(_, _, _, _, _, _, _, _) =>
        Json.obj("gmosSSpectroscopy" -> a.asJson)
      case a @ GmosNorthImaging(_, _, _, _)                  =>
        Json.obj("gmosNImaging" -> a.asJson)
      case a @ GmosSouthImaging(_, _, _, _)                  =>
        Json.obj("gmosSImaging" -> a.asJson)
      case a @ Flamingos2Spectroscopy(_, _, _, _, _)         =>
        Json.obj("flamingos2Spectroscopy" -> a.asJson)
      case a @ Flamingos2Imaging(_, _, _)                    =>
        Json.obj("flamingos2Imaging" -> a.asJson)
      case a @ Igrins2Spectroscopy(_, _)                     =>
        Json.obj("igrins2Spectroscopy" -> a.asJson)

  given Eq[InstrumentMode] with
    def eqv(x: InstrumentMode, y: InstrumentMode): Boolean =
      (x, y) match
        case (x0: GmosNorthSpectroscopy, y0: GmosNorthSpectroscopy)   => x0 === y0
        case (x0: GmosSouthSpectroscopy, y0: GmosSouthSpectroscopy)   => x0 === y0
        case (x0: GmosNorthImaging, y0: GmosNorthImaging)             => x0 === y0
        case (x0: GmosSouthImaging, y0: GmosSouthImaging)             => x0 === y0
        case (x0: Flamingos2Spectroscopy, y0: Flamingos2Spectroscopy) => x0 === y0
        case (x0: Flamingos2Imaging, y0: Flamingos2Imaging)           => x0 === y0
        case (x0: Igrins2Spectroscopy, y0: Igrins2Spectroscopy)       => x0 === y0
        case _                                                        => false
}
