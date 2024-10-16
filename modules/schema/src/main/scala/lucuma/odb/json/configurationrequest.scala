// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.syntax.all.*
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.CloudExtinction
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.ImageQuality
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.math.Coordinates
import lucuma.core.model.Configuration
import lucuma.core.model.Configuration.Conditions
import lucuma.core.model.Configuration.ObservingMode
import lucuma.core.model.Configuration.ObservingMode.GmosNorthLongSlit
import lucuma.core.model.Configuration.ObservingMode.GmosSouthLongSlit
import lucuma.core.model.ConfigurationRequest
import lucuma.odb.json.coordinates.query.given

object configurationrequest:

  trait QueryCodec:
    object DecodingFailures:
      val NoReferenceCoordinates = DecodingFailure("Reference coordinates are undefined.", Nil)
      val NoObservingMode = DecodingFailure("Observing mode is undefined.", Nil)

    given Decoder[Conditions] = hc =>
      for 
        c <- hc.downField("cloudExtinction").as[CloudExtinction]
        i <- hc.downField("imageQuality").as[ImageQuality]
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

    given Decoder[ObservingMode] = hc =>
      hc.downField("gmosNorthLongSlit").as(DecodeGmosNorthLongSlit) orElse
      hc.downField("gmosSouthLongSlit").as(DecodeGmosSouthLongSlit)

    given Encoder[ObservingMode] = m => 
      Json.obj(
        "gmosNorthLongSlit" -> Json.Null, // one of these will be replaced below
        "gmosSouthLongSlit" -> Json.Null, // one of these will be replaced below
        m match
          case GmosNorthLongSlit(grating) => "gmosNorthLongSlit" -> Json.obj("grating" -> grating.asJson)
          case GmosSouthLongSlit(grating) => "gmosSouthLongSlit" -> Json.obj("grating" -> grating.asJson)
      )

    /** A decoder based on the GraphQL schema, used for recursive service queries. */
    given Decoder[Configuration] = hc =>
      (
        hc.downField("conditions").as[Conditions],
        hc.downField("referenceCoordinates").as[Option[Coordinates]],
        hc.downField("observingMode").as[Option[ObservingMode]]
      ).tupled.flatMap:
        case (conds, Some(coords), Some(mode)) => Right(Configuration(conds, coords, mode))
        case (conds, None, _)                  => Left(DecodingFailures.NoReferenceCoordinates)
        case (conds, _, None)                  => Left(DecodingFailures.NoObservingMode)
      
    given Encoder[Configuration] = c =>
      Json.obj(
        "conditions" -> c.conditions.asJson,
        "referenceCoordinates" -> c.refererenceCoordinates.asJson,
        "observingMode" -> c.observingMode.asJson
      )

    given Decoder[ConfigurationRequest] = hc =>
      for
        id <- hc.downField("id").as[ConfigurationRequest.Id]
        st <- hc.downField("status").as[ConfigurationRequestStatus]
        cf <- hc.downField("configuration").as[Configuration]
      yield ConfigurationRequest(id, st, cf)

    given Encoder[ConfigurationRequest] = cr =>
      Json.obj(
        "id" -> cr.id.asJson,
        "status" -> cr.status.asJson,
        "configuration" -> cr.configuration.asJson
      )

  object query extends QueryCodec
