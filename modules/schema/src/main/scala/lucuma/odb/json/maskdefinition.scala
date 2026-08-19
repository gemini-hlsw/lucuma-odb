// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.syntax.eq.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.refined.given
import io.circe.syntax.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.MosDispersionDirection
import lucuma.core.enums.MosSlitPriority
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Coordinates
import lucuma.core.math.HourAngle
import lucuma.core.math.Redshift
import lucuma.core.math.syntax.units.*
import lucuma.core.model.mos.MosMaskProvenance
import lucuma.core.model.mos.MosObjectId
import lucuma.core.util.Timestamp
import lucuma.odb.data.MaskDefinition
import lucuma.odb.data.MaskSlit
import lucuma.odb.json.angle.query.given
import lucuma.odb.json.coordinates.query.given

/**
 * Codec for the mask definition blob stored on a MOS mask attachment.  The
 * encoding is the query format served directly by the GraphQL `mask` field,
 * so angles carry every unit representation and enums their GraphQL names.
 */
trait MaskDefinitionCodec:

  // MosSlitPriority tags are the mask file format's single characters
  // ("0".."3", "X"), which are not legal GraphQL enum values, so this codec
  // spells out the schema's enum names instead of using the Enumerated codec.
  extension (p: MosSlitPriority)
    private def priorityName: String =
      p match
        case MosSlitPriority.Acquisition => "ACQUISITION"
        case MosSlitPriority.High        => "HIGH"
        case MosSlitPriority.Medium      => "MEDIUM"
        case MosSlitPriority.Low         => "LOW"
        case MosSlitPriority.Ignore      => "IGNORE"

  given EncoderMosSlitPriority: Encoder[MosSlitPriority] =
    Encoder[String].contramap(_.priorityName)

  given Decoder[MosSlitPriority] =
    Decoder[String].emap: s =>
      MosSlitPriority.values
        .find(_.priorityName === s)
        .toRight(s"Could not parse MOS slit priority '$s'")

  // Spelled out as the names a future GraphQL enum would use, like the
  // priority above.
  extension (d: MosDispersionDirection)
    private def directionName: String =
      d match
        case MosDispersionDirection.Horizontal => "HORIZONTAL"
        case MosDispersionDirection.Vertical   => "VERTICAL"

  given EncoderMosDispersionDirection: Encoder[MosDispersionDirection] =
    Encoder[String].contramap(_.directionName)

  given Decoder[MosDispersionDirection] =
    Decoder[String].emap: s =>
      MosDispersionDirection.values
        .find(_.directionName === s)
        .toRight(s"Could not parse MOS dispersion direction '$s'")

  given Encoder[BrightnessValue] =
    Encoder[BigDecimal].contramap(_.value.value)

  given Decoder[BrightnessValue] =
    Decoder[BigDecimal].emap(BrightnessValue.from)

  given Encoder[Redshift] =
    Encoder[BigDecimal].contramap(_.z)

  given Decoder[Redshift] =
    Decoder[BigDecimal].map(Redshift(_))

  given Encoder[MosMaskProvenance] =
    Encoder.instance: p =>
      Json.obj(
        "softwareVersion"        -> p.softwareVersion.asJson,
        "designer"               -> p.designer.asJson,
        "designedAt"             -> p.designedAt.asJson,
        "sourceObjectTable"      -> p.sourceObjectTable.asJson,
        "detectorIdImaging"      -> p.detectorIdImaging.asJson,
        "detectorIdSpectroscopy" -> p.detectorIdSpectroscopy.asJson
      )

  given Decoder[MosMaskProvenance] =
    Decoder.instance: c =>
      for
        version <- c.downField("softwareVersion").as[Option[String]]
        who     <- c.downField("designer").as[Option[String]]
        when    <- c.downField("designedAt").as[Option[Timestamp]]
        table   <- c.downField("sourceObjectTable").as[Option[String]]
        img     <- c.downField("detectorIdImaging").as[Option[String]]
        spec    <- c.downField("detectorIdSpectroscopy").as[Option[String]]
      yield MosMaskProvenance(version, who, when, table, img, spec)

  private def signedAngle(a: Angle): Json =
    val µas = Angle.signedMicroarcseconds.get(a)
    val µs  = BigDecimal(µas) / 15
    val hms = HourAngle.HMS(Angle.hourAngle.get(Angle.fromMicroarcseconds(µas.abs))).format
    Json.obj(
      "microarcseconds" -> µas.asJson,
      "microseconds"    -> µs.asJson,
      "milliarcseconds" -> BigDecimal(µas, 3).asJson,
      "milliseconds"    -> (µs / 1_000).asJson,
      "arcseconds"      -> BigDecimal(µas, 6).asJson,
      "seconds"         -> (µs / 1_000_000).asJson,
      "arcminutes"      -> (BigDecimal(µas, 6) / 60).asJson,
      "minutes"         -> (µs / 60_000_000).asJson,
      "degrees"         -> (BigDecimal(µas, 6) / 3_600).asJson,
      "hours"           -> (µs / 3_600_000_000L).asJson,
      "dms"             -> Angle.fromStringSignedDMS.reverseGet(a).asJson,
      "hms"             -> (if µas < 0 then s"-$hms" else hms).asJson
    )

  private val signedAngleDecoder: Decoder[Angle] =
    Decoder.instance:
      _.downField("microarcseconds").as[Long].map(Angle.signedMicroarcseconds.reverseGet)

  given Encoder[MaskSlit] =
    Encoder.instance: s =>
      Json.obj(
        "id"               -> s.id.value.asJson,
        "coordinates"      -> s.coordinates.asJson,
        "x"                -> s.x.asJson,
        "y"                -> s.y.asJson,
        "width"            -> s.width.asJson,
        "length"           -> s.length.asJson,
        "offsetAlongSlit"  -> signedAngle(s.offsetAlongSlit),
        "offsetAcrossSlit" -> signedAngle(s.offsetAcrossSlit),
        "tilt"             -> signedAngle(s.tilt),
        "priority"         -> s.priority.asJson,
        "magnitude"        -> s.magnitude.asJson,
        "redshift"         -> s.redshift.asJson
      )

  given Decoder[MaskSlit] =
    Decoder.instance: c =>
      for
        id       <- c.downField("id").as[Int]
        coords   <- c.downField("coordinates").as[Coordinates]
        x        <- c.downField("x").as[BigDecimal]
        y        <- c.downField("y").as[BigDecimal]
        width    <- c.downField("width").as[Angle]
        length   <- c.downField("length").as[Angle]
        along    <- c.downField("offsetAlongSlit").as(using signedAngleDecoder)
        across   <- c.downField("offsetAcrossSlit").as(using signedAngleDecoder)
        tilt     <- c.downField("tilt").as(using signedAngleDecoder)
        priority <- c.downField("priority").as[MosSlitPriority]
        mag      <- c.downField("magnitude").as[BrightnessValue]
        z        <- c.downField("redshift").as[Option[Redshift]]
      yield MaskSlit(MosObjectId(id), coords, x, y, width, length, along, across, tilt, priority, mag, z)

  given Encoder[MaskDefinition] =
    Encoder.instance: d =>
      Json.obj(
        "name"                 -> d.name.asJson,
        "instrument"           -> d.instrument.asJson,
        "pixelScale"           -> d.pixelScale.value.asJson,
        "pointing"             -> d.pointing.asJson,
        "positionAngle"        -> d.positionAngle.asJson,
        "dispersionDirection"  -> d.dispersionDirection.asJson,
        "hasTiltedSlits"       -> d.hasTiltedSlits.asJson,
        "provenance"           -> d.provenance.asJson,
        "slits"                -> d.slits.asJson,
        "scienceSlitCount"     -> d.scienceSlits.length.asJson,
        "acquisitionSlitCount" -> d.acquisitionSlits.length.asJson,
        "averageSlitWidth"     -> d.averageSlitWidth.asJson
      )

  given Decoder[MaskDefinition] =
    Decoder.instance: c =>
      for
        name          <- c.downField("name").as[NonEmptyString]
        instrument    <- c.downField("instrument").as[Instrument]
        pixelScale    <- c.downField("pixelScale").as[BigDecimal]
        pointing      <- c.downField("pointing").as[Coordinates]
        positionAngle <- c.downField("positionAngle").as[Angle]
        direction     <- c.downField("dispersionDirection").as[MosDispersionDirection]
        tilted        <- c.downField("hasTiltedSlits").as[Boolean]
        provenance    <- c.downField("provenance").as[MosMaskProvenance]
        slits         <- c.downField("slits").as[List[MaskSlit]]
      yield MaskDefinition(name, instrument, pixelScale.pixelScale, pointing, positionAngle, direction, tilted, provenance, slits)

object maskDefinition extends MaskDefinitionCodec
