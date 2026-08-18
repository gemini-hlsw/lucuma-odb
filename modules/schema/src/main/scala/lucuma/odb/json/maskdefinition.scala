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
import lucuma.core.enums.MosSlitPriority
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.HourAngle
import lucuma.core.math.syntax.units.*
import lucuma.core.model.mos.MosObjectId
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
        "priority"         -> s.priority.asJson
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
      yield MaskSlit(MosObjectId(id), coords, x, y, width, length, along, across, tilt, priority)

  given Encoder[MaskDefinition] =
    Encoder.instance: d =>
      Json.obj(
        "name"                 -> d.name.asJson,
        "instrument"           -> d.instrument.asJson,
        "pixelScale"           -> d.pixelScale.value.asJson,
        "pointing"             -> d.pointing.asJson,
        "positionAngle"        -> d.positionAngle.asJson,
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
        slits         <- c.downField("slits").as[List[MaskSlit]]
      yield MaskDefinition(name, instrument, pixelScale.pixelScale, pointing, positionAngle, slits)

object maskDefinition extends MaskDefinitionCodec
