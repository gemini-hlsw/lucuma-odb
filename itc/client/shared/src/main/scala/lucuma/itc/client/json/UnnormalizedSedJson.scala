// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client.json

import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.UnnormalizedSED
import lucuma.itc.client.json.encoders.given

import syntax.*

given Encoder[UnnormalizedSED] = {
  case UnnormalizedSED.StellarLibrary(s) =>
    Json.obj("stellarLibrary" -> s.asScreamingJson)

  case UnnormalizedSED.CoolStarModel(s) =>
    Json.obj("coolStar" -> s.asScreamingJson)

  case UnnormalizedSED.Galaxy(s) =>
    Json.obj("galaxy" -> s.asScreamingJson)

  case UnnormalizedSED.Planet(s) =>
    Json.obj("planet" -> s.asScreamingJson)

  case UnnormalizedSED.Quasar(s) =>
    Json.obj("quasar" -> s.asScreamingJson)

  case UnnormalizedSED.HIIRegion(s) =>
    Json.obj("hiiRegion" -> s.asScreamingJson)

  case UnnormalizedSED.PlanetaryNebula(s) =>
    Json.obj("planetaryNebula" -> s.asScreamingJson)

  case UnnormalizedSED.PowerLaw(index) =>
    Json.obj("powerLaw" -> index.asJson)

  case UnnormalizedSED.BlackBody(temperature) =>
    Json.obj("blackBodyTempK" -> temperature.value.value.asJson)

  case UnnormalizedSED.UserDefined(fs) =>
    Json.obj(
      "fluxDensities" ->
        Json.arr(fs.toNel.toList.map { case (w, d) =>
          Json.obj(
            "wavelength" -> w.asJson,
            "density"    -> d.asJson
          )
        }*)
    )

  case UnnormalizedSED.UserDefinedAttachment(attachmentId) =>
    Json.obj("fluxDensitiesAttachment" -> attachmentId.asJson)
}
