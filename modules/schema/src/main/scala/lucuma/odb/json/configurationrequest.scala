// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.math.Coordinates
import lucuma.core.model.CloudExtinction
import lucuma.core.model.Configuration
import lucuma.core.model.Configuration.Conditions
import lucuma.core.model.Configuration.ObservingMode
import lucuma.core.model.Configuration.ObservingMode.*
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.ImageQuality
import lucuma.odb.json.coordinates.query.given
import lucuma.odb.json.region.query.given
import lucuma.core.math.Region
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.Flamingos2Disperser

object configurationrequest:

  trait QueryCodec:
    object DecodingFailures:
      val NoReferenceCoordinates = DecodingFailure("Reference coordinates are undefined.", Nil)
      val NoObservingMode = DecodingFailure("Observing mode is undefined.", Nil)

    given Decoder[Conditions] = hc =>
      for 
        c <- hc.downField("cloudExtinction").as[CloudExtinction.Preset]
        i <- hc.downField("imageQuality").as[ImageQuality.Preset]
        s <- hc.downField("skyBackground").as[SkyBackground]
        w <- hc.downField("waterVapor").as[WaterVapor]
      yield Conditions(c, i, s, w)

    given Encoder[Conditions] = c =>
      Json.obj(
        "cloudExtinction" -> c.cloudExtinction.asJson,
        "imageQuality" -> c.imageQuality.asJson,
        "skyBackground" -> c.skyBackground.asJson,
        "waterVapor" -> c.waterVapor.asJson,
      )

    val DecodeGmosNorthLongSlit: Decoder[GmosNorthLongSlit] = hc =>
      hc.downField("grating").as[GmosNorthGrating].map(GmosNorthLongSlit(_))

    val DecodeGmosSouthLongSlit: Decoder[GmosSouthLongSlit] = hc =>
      hc.downField("grating").as[GmosSouthGrating].map(GmosSouthLongSlit(_))

    val DecodeGmosNorthImaging: Decoder[GmosNorthImaging] = hc =>
      hc.downField("filters").as[List[GmosNorthFilter]].map(GmosNorthImaging(_))

    val DecodeGmosSouthImaging: Decoder[GmosSouthImaging] = hc =>
      hc.downField("filters").as[List[GmosSouthFilter]].map(GmosSouthImaging(_))

    val DecodeFlamingos2LongSlit: Decoder[Flamingos2LongSlit] = hc =>
      hc.downField("disperser").as[Flamingos2Disperser].map(Flamingos2LongSlit(_))

    given Decoder[ObservingMode] = hc =>
      hc.downField("gmosNorthLongSlit").as(using DecodeGmosNorthLongSlit) orElse
      hc.downField("gmosSouthLongSlit").as(using DecodeGmosSouthLongSlit) orElse
      hc.downField("gmosNorthImaging").as(using DecodeGmosNorthImaging) orElse
      hc.downField("gmosSouthImaging").as(using DecodeGmosSouthImaging) orElse
      hc.downField("flamingos2LongSlit").as(using DecodeFlamingos2LongSlit) orElse
      Left(DecodingFailure(s"couldn't decode mode: ${hc.top}", Nil))

    given Encoder[ObservingMode] = m => 
      Json.obj(
        "gmosNorthLongSlit" -> Json.Null, // one of these will be replaced below
        "gmosSouthLongSlit" -> Json.Null, // one of these will be replaced below
        "gmosNorthImaging" -> Json.Null, // one of these will be replaced below
        "gmosSouthImaging" -> Json.Null, // one of these will be replaced below
        "flamingos2LongSlit" -> Json.Null, // one of these will be replaced below
        m match
          case GmosNorthLongSlit(grating) => "gmosNorthLongSlit" -> Json.obj("grating" -> grating.asJson)
          case GmosSouthLongSlit(grating) => "gmosSouthLongSlit" -> Json.obj("grating" -> grating.asJson)
          case GmosNorthImaging(filter) => "gmosNorthImaging" -> Json.obj("filter" -> filter.asJson)
          case GmosSouthImaging(filter) => "gmosSouthImaging" -> Json.obj("filter" -> filter.asJson)
          case Flamingos2LongSlit(disperser) => "flamingos2LongSlit" -> Json.obj("disperser" -> disperser.asJson)
      )

    given Encoder[Either[Coordinates, Region]] = e =>
      Json.obj(
        "coordinates" -> e.left.toOption.asJson,
        "region" -> e.toOption.asJson,
      )

    given Decoder[Option[Either[Coordinates, Region]]] = hc =>
      (
        hc.downField("coordinates").as[Option[Coordinates]],
        hc.downField("region").as[Option[Region]]
      ).tupled.flatMap:
        case (Some(c), None)    => Left(c).some.asRight
        case (None, Some(r))    => Right(r).some.asRight
        case (None, None)       => None.asRight
        case (Some(_), Some(_)) => Left(DecodingFailure("Cannot decode target; both coords and region are defined.", Nil))

    /** A decoder based on the GraphQL schema, used for recursive service queries. */
    given Decoder[Configuration] = hc =>
      (
        hc.downField("conditions").as[Conditions],
        hc.downField("target").as[Option[Either[Coordinates, Region]]], // may be missing
        hc.downField("observingMode").as[Option[ObservingMode]]
      ).tupled.flatMap:
        case (conds, Some(coords), Some(mode)) => Right(Configuration(conds, coords, mode))
        case (conds, None, _)                  => Left(DecodingFailures.NoReferenceCoordinates)
        case (conds, _, None)                  => Left(DecodingFailures.NoObservingMode)
      
    given Encoder[Configuration] = c =>
      Json.obj(
        "conditions" -> c.conditions.asJson,
        "target" -> c.target.asJson,
        "observingMode" -> c.observingMode.asJson
      )

    given Decoder[ConfigurationRequest] = hc =>
      for
        id <- hc.downField("id").as[ConfigurationRequest.Id]
        st <- hc.downField("status").as[ConfigurationRequestStatus]
        ju <- hc.downField("justification").as[Option[NonEmptyString]]
        cf <- hc.downField("configuration").as[Configuration]
      yield ConfigurationRequest(id, st, ju, cf)

    given Encoder[ConfigurationRequest] = cr =>
      Json.obj(
        "id" -> cr.id.asJson,
        "status" -> cr.status.asJson,
        "justification" -> cr.justification.asJson,
        "configuration" -> cr.configuration.asJson
      )

  object query extends QueryCodec
