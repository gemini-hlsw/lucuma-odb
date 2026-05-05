// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import cats.derived.*
import cats.syntax.functor.*
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.enums.PortDisposition
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.itc.ItcGhostDetector
import lucuma.itc.client.json.encoders.given
import lucuma.itc.client.json.syntax.*
import lucuma.odb.json.gmos.given
import lucuma.odb.json.wavelength.transport.given
import monocle.Prism
import monocle.macros.GenPrism

sealed trait InstrumentMode derives Eq:
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
    readMode:         Flamingos2ReadMode,
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
          "readMode"         -> a.readMode.asScreamingJson,
          "port"             -> a.port.asScreamingJson
        )
      )

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

  case class GhostSpectroscopy(
    resolutionMode: GhostResolutionMode,
    redDetector:    ItcGhostDetector,
    blueDetector:   ItcGhostDetector
  ) extends InstrumentMode derives Eq:
    // This will not be used by the OCS ITC, but is required to meet the API
    val exposureTimeMode: ExposureTimeMode = redDetector.timeAndCount
    override def displayName: String       = "GHOST Spectroscopy"

  object GhostSpectroscopy:
    given Encoder[ItcGhostDetector] = a =>
      Json.fromFields(
        List(
          "timeAndCount" -> a.timeAndCount.asJson,
          "readMode"     -> a.readMode.asJson,
          "binning"      -> a.binning.asJson
        )
      )

    given Encoder[GhostSpectroscopy] = a =>
      Json.fromFields(
        List(
          "resolutionMode" -> a.resolutionMode.asScreamingJson,
          "redDetector"    -> a.redDetector.asJson,
          "blueDetector"   -> a.blueDetector.asJson
        )
      )

  case class GnirsSpectroscopy(
    exposureTimeMode:  ExposureTimeMode.TimeAndCountMode,
    centralWavelength: Wavelength,
    filter:            GnirsFilter,
    slitWidth:         GnirsFpuSlit,
    prism:             GnirsPrism,
    grating:           GnirsGrating,
    camera:            GnirsCamera,
    readMode:          GnirsReadMode,
    wellDepth:         GnirsWellDepth,
    port:              PortDisposition = PortDisposition.Bottom
  ) extends InstrumentMode derives Eq:
    override def displayName: String =
      "GNIRS Spectroscopy"

  object GnirsSpectroscopy:
    given Encoder[GnirsSpectroscopy] = a =>
      Json.obj(
        "timeAndCount"      -> a.exposureTimeMode.asJson,
        "centralWavelength" -> a.centralWavelength.asJson,
        "filter"            -> a.filter.asScreamingJson,
        "slitWidth"         -> a.slitWidth.asScreamingJson,
        "prism"             -> a.prism.asScreamingJson,
        "grating"           -> a.grating.asScreamingJson,
        "camera"            -> a.camera.asScreamingJson,
        "readMode"          -> a.readMode.asScreamingJson,
        "wellDepth"         -> a.wellDepth.asScreamingJson,
        "port"              -> a.port.asScreamingJson
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
    readMode:         Flamingos2ReadMode,
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
          "readMode"         -> a.readMode.asScreamingJson,
          "port"             -> a.port.asScreamingJson
        )
      )

  val gmosNorthSpectroscopy: Prism[InstrumentMode, GmosNorthSpectroscopy] =
    GenPrism[InstrumentMode, GmosNorthSpectroscopy]

  val gmosSouthSpectroscopy: Prism[InstrumentMode, GmosSouthSpectroscopy] =
    GenPrism[InstrumentMode, GmosSouthSpectroscopy]

  val flamingos2Spectroscopy: Prism[InstrumentMode, Flamingos2Spectroscopy] =
    GenPrism[InstrumentMode, Flamingos2Spectroscopy]

  val igrins2Spectroscopy: Prism[InstrumentMode, Igrins2Spectroscopy] =
    GenPrism[InstrumentMode, Igrins2Spectroscopy]

  val ghostSpectroscopy: Prism[InstrumentMode, GhostSpectroscopy] =
    GenPrism[InstrumentMode, GhostSpectroscopy]

  val gnirsSpectroscopy: Prism[InstrumentMode, GnirsSpectroscopy] =
    GenPrism[InstrumentMode, GnirsSpectroscopy]

  val gmosNorthImaging: Prism[InstrumentMode, GmosNorthImaging] =
    GenPrism[InstrumentMode, GmosNorthImaging]

  val gmosSouthImaging: Prism[InstrumentMode, GmosSouthImaging] =
    GenPrism[InstrumentMode, GmosSouthImaging]

  val flamingos2Imaging: Prism[InstrumentMode, Flamingos2Imaging] =
    GenPrism[InstrumentMode, Flamingos2Imaging]

  given Encoder[InstrumentMode] = a =>
    a match
      case a @ GmosNorthSpectroscopy(_, _, _, _, _, _, _, _)   =>
        Json.obj("gmosNSpectroscopy" -> a.asJson)
      case a @ GmosSouthSpectroscopy(_, _, _, _, _, _, _, _)   =>
        Json.obj("gmosSSpectroscopy" -> a.asJson)
      case a @ GmosNorthImaging(_, _, _, _)                    =>
        Json.obj("gmosNImaging" -> a.asJson)
      case a @ GmosSouthImaging(_, _, _, _)                    =>
        Json.obj("gmosSImaging" -> a.asJson)
      case a @ Flamingos2Spectroscopy(_, _, _, _, _, _)        =>
        Json.obj("flamingos2Spectroscopy" -> a.asJson)
      case a @ Flamingos2Imaging(_, _, _, _)                   =>
        Json.obj("flamingos2Imaging" -> a.asJson)
      case a @ Igrins2Spectroscopy(_, _)                       =>
        Json.obj("igrins2Spectroscopy" -> a.asJson)
      case a @ GhostSpectroscopy(_, _, _)                      =>
        Json.obj("ghostSpectroscopy" -> a.asJson)
      case a @ GnirsSpectroscopy(_, _, _, _, _, _, _, _, _, _) =>
        Json.obj("gnirsSpectroscopy" -> a.asJson)
}
