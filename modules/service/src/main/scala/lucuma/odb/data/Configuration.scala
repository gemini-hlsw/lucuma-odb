// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data
import cats.Eq
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.Encoder
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.math.Coordinates
import lucuma.odb.json.coordinates.query.given

case class Configuration(conditions: Configuration.Conditions, refererenceCoordinates: Coordinates, observingMode: Configuration.ObservingMode):
  def subsumes(other: Configuration): Boolean =
    if observingMode === other.observingMode then
      ???
    else false
    

object Configuration:

  given Decoder[Configuration] = hc =>
    (
      hc.downField("conditions").as[Conditions],
      hc.downField("referenceCoordinates").as[Coordinates],
      hc.downField("observingMode").as[ObservingMode]
    ).mapN(apply)

  given Encoder[Configuration] = ???

  case class Conditions(  
  )
  object Conditions:
    given Decoder[Conditions] =
      ???

  enum ObservingMode:
    case GmosNorthLongSlit(grating: GmosNorthGrating)
    case GmosSouthLongSlit(grating: GmosSouthGrating)

  object ObservingMode:
    given Decoder[ObservingMode] =
      ???

    given Eq[ObservingMode] =
      ???