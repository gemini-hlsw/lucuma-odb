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
import lucuma.odb.data.Configuration.ObservingMode.GmosNorthLongSlit
import lucuma.odb.data.Configuration.ObservingMode.GmosSouthLongSlit
import lucuma.odb.json.coordinates.query.given

case class Configuration(conditions: Configuration.Conditions, refererenceCoordinates: Coordinates, observingMode: Configuration.ObservingMode):
  def subsumes(other: Configuration): Boolean =
    if observingMode === other.observingMode then
      ???
    else false    

object Configuration:

  /** A decoder based on the GraphQL schema, used for recursive service queries. */
  given Decoder[Configuration] = hc =>
    (
      hc.downField("conditions").as[Conditions],
      hc.downField("referenceCoordinates").as[Coordinates],
      hc.downField("observingMode").as[ObservingMode]
    ).mapN(apply)

  given Encoder[Configuration] = ???

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

  sealed abstract class ObservingMode(val tpe: ObservingModeType):
    def gmosNorthLongSlit: Option[GmosNorthLongSlit] = Some(this).collect { case m: GmosNorthLongSlit => m }
    def gmosSouthLongSlit: Option[GmosSouthLongSlit] = Some(this).collect { case m: GmosSouthLongSlit => m }

  object ObservingMode:

    case class GmosNorthLongSlit(grating: GmosNorthGrating) extends ObservingMode(ObservingModeType.GmosNorthLongSlit)
    case class GmosSouthLongSlit(grating: GmosSouthGrating) extends ObservingMode(ObservingModeType.GmosSouthLongSlit)

    val DecodeGmosNorthLongSlit: Decoder[GmosNorthLongSlit] = hc =>
      hc.downField("grating").as[GmosNorthGrating].map(GmosNorthLongSlit(_))

    val DecodeGmosSouthLongSlit: Decoder[GmosSouthLongSlit] = hc =>
      hc.downField("grating").as[GmosSouthGrating].map(GmosSouthLongSlit(_))

    given Decoder[ObservingMode] = hc =>
      hc.downField("gmosNorthLongSlit").as(DecodeGmosNorthLongSlit) orElse
      hc.downField("gmosSouthLongSlit").as(DecodeGmosSouthLongSlit)

    given Eq[ObservingMode] =
      ???