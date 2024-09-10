// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data
import cats.Eq
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.Encoder
import lucuma.core.enums.CloudExtinction
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.ImageQuality
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.math.Coordinates
import lucuma.core.math.Angle
import lucuma.odb.data.Configuration.ObservingMode.GmosNorthLongSlit
import lucuma.odb.data.Configuration.ObservingMode.GmosSouthLongSlit
import lucuma.odb.json.coordinates.query.given
import io.circe.DecodingFailure
import cats.kernel.Order
import io.circe.Json
import io.circe.syntax.*

case class Configuration(conditions: Configuration.Conditions, refererenceCoordinates: Coordinates, observingMode: Configuration.ObservingMode):
  def subsumes(other: Configuration): Boolean =
    conditions >= other.conditions &&
    observingMode.fov.toDoubleDegrees / 2.0 >= refererenceCoordinates.angularDistance(other.refererenceCoordinates).toDoubleDegrees &&
    observingMode === other.observingMode

object Configuration:

  object DecodingFailures:
    val NoReferenceCoordinates = DecodingFailure("Reference coordinates are undefined.", Nil)
    val NoObservingMode = DecodingFailure("Observing mode is undefined.", Nil)

  /** A decoder based on the GraphQL schema, used for recursive service queries. */
  given Decoder[Configuration] = hc =>
    (
      hc.downField("conditions").as[Conditions],
      hc.downField("referenceCoordinates").as[Option[Coordinates]],
      hc.downField("observingMode").as[Option[ObservingMode]]
    ).tupled.flatMap:
      case (conds, Some(coords), Some(mode)) => Right(apply(conds, coords, mode))
      case (conds, None, _)                  => Left(DecodingFailures.NoReferenceCoordinates)
      case (conds, _, None)                  => Left(DecodingFailures.NoObservingMode)
      
  given Encoder[Configuration] = c =>
    Json.obj(
      "conditions" -> c.conditions.asJson,
      "referenceCoordinates" -> c.refererenceCoordinates.asJson,
      "observingMode" -> c.observingMode.asJson
    )

  case class Conditions(
    cloudExtinction: CloudExtinction,
    imageQuality: ImageQuality,
    skyBackground: SkyBackground,
    waterVapor: WaterVapor,
  )

  object Conditions:

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

    given Order[Conditions] = Order.by: conds =>
      (conds.cloudExtinction, conds.imageQuality, conds.skyBackground, conds.waterVapor)

  sealed abstract class ObservingMode(val tpe: ObservingModeType, val fov: Angle):
    def gmosNorthLongSlit: Option[GmosNorthLongSlit] = Some(this).collect { case m: GmosNorthLongSlit => m }
    def gmosSouthLongSlit: Option[GmosSouthLongSlit] = Some(this).collect { case m: GmosSouthLongSlit => m }

  object ObservingMode:

    case class GmosNorthLongSlit(grating: GmosNorthGrating) extends ObservingMode(ObservingModeType.GmosNorthLongSlit, Angle.fromDoubleArcseconds(1.23)) // TODO
    case class GmosSouthLongSlit(grating: GmosSouthGrating) extends ObservingMode(ObservingModeType.GmosSouthLongSlit, Angle.fromDoubleArcseconds(1.23)) // TODO

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

    given Eq[ObservingMode] =
      Eq.instance:
        case (GmosNorthLongSlit(g1), GmosNorthLongSlit(g2)) => g1 === g2
        case (GmosSouthLongSlit(g1), GmosSouthLongSlit(g2)) => g1 === g2
        case _ => false
