// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import io.circe.*
import io.circe.syntax.*
import lucuma.core.math.RadialVelocity
import lucuma.core.model.SourceProfile
import lucuma.itc.client.json.given

case class TargetInput(sourceProfile: SourceProfile, radialVelocity: RadialVelocity)

object TargetInput:
  given Encoder.AsObject[TargetInput] = a =>
    JsonObject(
      "sourceProfile"  -> a.sourceProfile.asJson,
      "radialVelocity" -> Json.obj(
        "metersPerSecond" -> RadialVelocity.fromMetersPerSecond
          .reverseGet(a.radialVelocity)
          .asJson
      )
    )

  given Eq[TargetInput] =
    Eq.by(a => (a.sourceProfile, a.radialVelocity))
