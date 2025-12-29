// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import cats.derived.*
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.functor.*
import io.circe.Decoder
import io.circe.DecodingFailure
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
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.itc.client.json.syntax.*
import lucuma.odb.json.gmos.given
import lucuma.odb.json.wavelength.transport.given
import monocle.Prism
import monocle.macros.GenPrism

sealed trait InstrumentMode:
  def displayName: String

object InstrumentMode {

  case class GmosNorthSpectroscopy(
    centralWavelength: Wavelength,
    grating:           GmosNorthGrating,
    filter:            Option[GmosNorthFilter],
    fpu:               GmosFpu.North,
    ccdMode:           Option[GmosCcdMode],
    roi:               Option[GmosRoi]
  ) extends InstrumentMode derives Eq:
    override def displayName: String =
      "GMOS North Spectroscopy"

  object GmosNorthSpectroscopy {

    given Encoder[GmosNorthSpectroscopy] = a =>
      Json.fromFields(
        List(
          "centralWavelength" -> a.centralWavelength.asJson,
          "grating"           -> a.grating.asScreamingJson,
          "fpu"               -> a.fpu.asJson,
          "ccdMode"           -> a.ccdMode.asJson,
          "roi"               -> a.roi.asJson
        ) ++ a.filter.map(_.asScreamingJson).tupleLeft("filter").toList
      )

    given Decoder[GmosNorthSpectroscopy] = c =>
      for
        cw <- c.downField("centralWavelength").as[Wavelength]
        g  <- c.downField("grating").as[GmosNorthGrating]
        f  <- c.downField("filter").as[Option[GmosNorthFilter]]
        u  <- c.downField("fpu").as[GmosFpu.North]
        d  <- c.downField("ccdMode").as[Option[GmosCcdMode]]
        r  <- c.downField("roi").as[Option[GmosRoi]]
      yield GmosNorthSpectroscopy(cw, g, f, u, d, r)

  }

  case class GmosSouthSpectroscopy(
    centralWavelength: Wavelength,
    grating:           GmosSouthGrating,
    filter:            Option[GmosSouthFilter],
    fpu:               GmosFpu.South,
    ccdMode:           Option[GmosCcdMode],
    roi:               Option[GmosRoi]
  ) extends InstrumentMode derives Eq:
    override def displayName: String =
      "GMOS South Spectroscopy"

  object GmosSouthSpectroscopy:

    given Encoder[GmosSouthSpectroscopy] = a =>
      Json.fromFields(
        List(
          "centralWavelength" -> a.centralWavelength.asJson,
          "grating"           -> a.grating.asScreamingJson,
          "fpu"               -> a.fpu.asJson,
          "ccdMode"           -> a.ccdMode.asJson,
          "roi"               -> a.roi.asJson
        ) ++ a.filter.map(_.asScreamingJson).tupleLeft("filter").toList
      )

    given Decoder[GmosSouthSpectroscopy] = c =>
      for
        cw <- c.downField("centralWavelength").as[Wavelength]
        g  <- c.downField("grating").as[GmosSouthGrating]
        f  <- c.downField("filter").as[Option[GmosSouthFilter]]
        u  <- c.downField("fpu").as[GmosFpu.South]
        d  <- c.downField("ccdMode").as[Option[GmosCcdMode]]
        r  <- c.downField("roi").as[Option[GmosRoi]]
      yield GmosSouthSpectroscopy(cw, g, f, u, d, r)

  case class Flamingos2Spectroscopy(
    disperser: Flamingos2Disperser,
    filter:    Flamingos2Filter,
    fpu:       Flamingos2Fpu
  ) extends InstrumentMode derives Eq:
    override def displayName: String =
      "Flamingos 2 Spectroscopy"

  object Flamingos2Spectroscopy:

    given Encoder[Flamingos2Spectroscopy] = a =>
      Json.fromFields(
        List(
          "disperser" -> a.disperser.asScreamingJson,
          "fpu"       -> a.fpu.asJson,
          "filter"    -> a.filter.asJson
        )
      )

    given Decoder[Flamingos2Spectroscopy] = c =>
      for
        g <- c.downField("disperser").as[Flamingos2Disperser]
        f <- c.downField("filter").as[Flamingos2Filter]
        u <- c.downField("fpu").as[Flamingos2Fpu]
      yield Flamingos2Spectroscopy(g, f, u)

  case class GmosNorthImaging(
    filter:  GmosNorthFilter,
    ccdMode: Option[GmosCcdMode]
  ) extends InstrumentMode derives Eq:
    override def displayName: String =
      "GMOS North Imaging"

  object GmosNorthImaging:

    given Encoder[GmosNorthImaging] = a =>
      Json.obj(
        "filter"  -> a.filter.asScreamingJson,
        "ccdMode" -> a.ccdMode.asJson
      )

    given Decoder[GmosNorthImaging] = c =>
      for
        f <- c.downField("filter").as[GmosNorthFilter]
        c <- c.downField("ccdMode").as[Option[GmosCcdMode]]
      yield GmosNorthImaging(f, c)

  case class GmosSouthImaging(
    filter:  GmosSouthFilter,
    ccdMode: Option[GmosCcdMode]
  ) extends InstrumentMode derives Eq:
    override def displayName: String =
      "GMOS South Imaging"

  object GmosSouthImaging:

    given Encoder[GmosSouthImaging] = a =>
      Json.obj(
        "filter"  -> a.filter.asScreamingJson,
        "ccdMode" -> a.ccdMode.asJson
      )

    given Decoder[GmosSouthImaging] = c =>
      for
        f <- c.downField("filter").as[GmosSouthFilter]
        c <- c.downField("ccdMode").as[Option[GmosCcdMode]]
      yield GmosSouthImaging(f, c)

  case class Flamingos2Imaging(
    filter: Flamingos2Filter
  ) extends InstrumentMode derives Eq:
    override def displayName: String =
      "Flamingos 2 Imaging"

  object Flamingos2Imaging:

    given Encoder[Flamingos2Imaging] = a =>
      Json.fromFields(
        List(
          "filter" -> a.filter.asJson
        )
      )

    given Decoder[Flamingos2Imaging] = c =>
      for f <- c.downField("filter").as[Flamingos2Filter]
      yield Flamingos2Imaging(f)

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

  given Encoder[InstrumentMode] = a =>
    a match
      case a @ GmosNorthSpectroscopy(_, _, _, _, _, _) =>
        Json.obj("gmosNSpectroscopy" -> a.asJson)
      case a @ GmosSouthSpectroscopy(_, _, _, _, _, _) =>
        Json.obj("gmosSSpectroscopy" -> a.asJson)
      case a @ GmosNorthImaging(_, _)                  =>
        Json.obj("gmosNImaging" -> a.asJson)
      case a @ GmosSouthImaging(_, _)                  =>
        Json.obj("gmosSImaging" -> a.asJson)
      case a @ Flamingos2Spectroscopy(_, _, _)         =>
        Json.obj("flamingos2Spectroscopy" -> a.asJson)
      case a @ Flamingos2Imaging(_)                    =>
        Json.obj("flamingos2Imaging" -> a.asJson)

  given Decoder[InstrumentMode] = c =>
    for
      ns <- c.downField("gmosNSpectroscopy").as[Option[GmosNorthSpectroscopy]]
      ss <- c.downField("gmosSSpectroscopy").as[Option[GmosSouthSpectroscopy]]
      ni <- c.downField("gmosNImaging").as[Option[GmosNorthImaging]]
      si <- c.downField("gmosSImaging").as[Option[GmosSouthImaging]]
      fs <- c.downField("flamingos2Spectroscopy").as[Option[Flamingos2Spectroscopy]]
      fi <- c.downField("flamingos2Imaging").as[Option[Flamingos2Imaging]]
      m  <- (ns, ss, ni, si, fs, fi) match
              case (Some(n), None, None, None, None, None) => (n: InstrumentMode).asRight
              case (None, Some(s), None, None, None, None) => (s: InstrumentMode).asRight
              case (None, None, Some(s), None, None, None) => (s: InstrumentMode).asRight
              case (None, None, None, Some(s), None, None) => (s: InstrumentMode).asRight
              case (None, None, None, None, Some(s), None) => (s: InstrumentMode).asRight
              case (None, None, None, None, None, Some(s)) => (s: InstrumentMode).asRight
              case _                                       =>
                DecodingFailure("Expected exactly one of 'gmosN' or 'gmosS' or 'flamingos2'.",
                                c.history
                ).asLeft
    yield m

  given Eq[InstrumentMode] with
    def eqv(x: InstrumentMode, y: InstrumentMode): Boolean =
      (x, y) match
        case (x0: GmosNorthSpectroscopy, y0: GmosNorthSpectroscopy)   => x0 === y0
        case (x0: GmosSouthSpectroscopy, y0: GmosSouthSpectroscopy)   => x0 === y0
        case (x0: GmosNorthImaging, y0: GmosNorthImaging)             => x0 === y0
        case (x0: GmosSouthImaging, y0: GmosSouthImaging)             => x0 === y0
        case (x0: Flamingos2Spectroscopy, y0: Flamingos2Spectroscopy) => x0 === y0
        case (x0: Flamingos2Imaging, y0: Flamingos2Imaging)           => x0 === y0
        case _                                                        => false
}
