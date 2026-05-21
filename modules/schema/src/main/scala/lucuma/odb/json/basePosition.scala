// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Encoder
import io.circe.Json
import io.circe.refined.given
import io.circe.syntax.*
import lucuma.core.model.Target
import lucuma.odb.data.BasePosition
import lucuma.odb.json.coordinates.query.given
import lucuma.odb.json.target.query.*

trait BasePositionCodec:

  given Encoder[BasePosition] =
    given Encoder[Target.Sidereal]    = siderealDefinitionEncoder
    given Encoder[Target.Nonsidereal] = nonsiderealDefinitionEncoder

    Encoder.instance: bp =>
      Json.obj(
        "type"        -> bp.basePositionType.asJson,
        "name"        -> bp.name.asJson,
        "sidereal"    -> bp.sidereal.fold(Json.Null)(_.asJson),
        "nonsidereal" -> bp.nonsidereal.fold(Json.Null)(_.asJson),
        "coordinates" -> bp.coordinates.asJson
      )

object basePosition extends BasePositionCodec
