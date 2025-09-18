// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import cats.derived.*
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.ImageQuality
import lucuma.core.util.Enumerated
import lucuma.itc.CloudExtinctionInput
import lucuma.itc.ImageQualityInput
import lucuma.itc.client.json.syntax.*
import monocle.Focus
import monocle.Lens

given Encoder[ImageQualityInput] =
  _.value match
    case Left(p)  => Json.obj("preset" -> p.asScreamingJson)
    case Right(v) => Json.obj("arcsec" -> v.asJson)

given Encoder[CloudExtinctionInput] =
  _.value match
    case Left(p)  => Json.obj("preset" -> p.asScreamingJson)
    case Right(v) => Json.obj("extinction" -> v.asJson)

case class ItcConstraintsInput(
  imageQuality:    ImageQualityInput,
  cloudExtinction: CloudExtinctionInput,
  skyBackground:   SkyBackground,
  waterVapor:      WaterVapor,
  elevationRange:  ElevationRange
) derives Eq

object ItcConstraintsInput:
  extension (cs: ConstraintSet) def toInput: ItcConstraintsInput = fromConstraintSet(cs)

  def fromConstraintSet(cs: ConstraintSet): ItcConstraintsInput =
    ItcConstraintsInput(
      imageQuality = ImageQualityInput.preset(cs.imageQuality),
      cloudExtinction = CloudExtinctionInput.preset(cs.cloudExtinction),
      skyBackground = cs.skyBackground,
      waterVapor = cs.waterVapor,
      elevationRange = cs.elevationRange
    )

  val imageQuality: Lens[ItcConstraintsInput, ImageQualityInput] =
    Focus[ItcConstraintsInput](_.imageQuality)

  val cloudExtinction: Lens[ItcConstraintsInput, CloudExtinctionInput] =
    Focus[ItcConstraintsInput](_.cloudExtinction)

  val skyBackground: Lens[ItcConstraintsInput, SkyBackground] =
    Focus[ItcConstraintsInput](_.skyBackground)

  val waterVapor: Lens[ItcConstraintsInput, WaterVapor] =
    Focus[ItcConstraintsInput](_.waterVapor)

  val elevationRange: Lens[ItcConstraintsInput, ElevationRange] =
    Focus[ItcConstraintsInput](_.elevationRange)

  given Encoder[ItcConstraintsInput] with
    def apply(a: ItcConstraintsInput): Json =
      Json
        .obj(
          "imageQuality"    -> a.imageQuality.asJson,
          "cloudExtinction" -> a.cloudExtinction.asJson,
          "skyBackground"   -> a.skyBackground.asScreamingJson,
          "waterVapor"      -> a.waterVapor.asScreamingJson,
          "elevationRange"  -> (a.elevationRange match {
            case ElevationRange.ByAirMass(min, max)             =>
              Json.obj(
                "airMass" ->
                  Json.obj(
                    "min" -> min.toBigDecimal.asJson,
                    "max" -> max.toBigDecimal.asJson
                  )
              )
            case ElevationRange.ByHourAngle(minHours, maxHours) =>
              Json.obj(
                "hourAngle" ->
                  Json.obj(
                    "minHours" -> minHours.toBigDecimal.asJson,
                    "maxHours" -> maxHours.toBigDecimal.asJson
                  )
              )
          })
        )
