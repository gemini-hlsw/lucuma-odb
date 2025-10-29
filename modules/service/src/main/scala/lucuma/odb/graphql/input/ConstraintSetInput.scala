// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.option.*
import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.ImageQuality
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.ConstraintSetInput.NominalConstraints

final case class ConstraintSetInput(
  cloudExtinction: Option[CloudExtinction.Preset],
  imageQuality:    Option[ImageQuality.Preset],
  skyBackground:   Option[SkyBackground],
  waterVapor:      Option[WaterVapor],
  elevationRange:  Option[ElevationRangeInput]
) {

  def create: Result[ConstraintSet] = {
    // TODO: default this (as below) or fail when missing?
    val ce = cloudExtinction.getOrElse(NominalConstraints.cloudExtinction)
    val iq = imageQuality.getOrElse(NominalConstraints.imageQuality)
    val sb = skyBackground.getOrElse(NominalConstraints.skyBackground)
    val wv = waterVapor.getOrElse(NominalConstraints.waterVapor)

    elevationRange.fold(Result(NominalConstraints.elevationRange))(_.create).map { er =>
      ConstraintSet(iq, ce, sb, wv, er)
    }
  }

}

object ConstraintSetInput {

  val NominalConstraints: ConstraintSet =
    ConstraintSet(
      cloudExtinction = CloudExtinction.Preset.PointThree,
      imageQuality    = ImageQuality.Preset.OnePointZero,
      skyBackground   = SkyBackground.Bright,
      waterVapor      = WaterVapor.Wet,
      elevationRange  = ElevationRange.ByAirMass.Default
    )

  val CalibrationConstraints: ConstraintSet =
    ConstraintSet(
      cloudExtinction = CloudExtinction.Preset.ThreePointZero,
      imageQuality    = ImageQuality.Preset.TwoPointZero,
      skyBackground   = SkyBackground.Bright,
      waterVapor      = WaterVapor.Wet,
      elevationRange  = ElevationRange.ByAirMass.Default
    )

  val SpecPhotoConstraints: ConstraintSet = CalibrationConstraints
  val TwilightConstraints: ConstraintSet = CalibrationConstraints.copy(cloudExtinction = CloudExtinction.Preset.PointThree)

  val Default: ConstraintSetInput =
    ConstraintSetInput(
      NominalConstraints.cloudExtinction.some,
      NominalConstraints.imageQuality.some,
      NominalConstraints.skyBackground.some,
      NominalConstraints.waterVapor.some,
      ElevationRangeInput.Default.some
    )

  val SpecPhotoCalibration: ConstraintSetInput =
    ConstraintSetInput(
      SpecPhotoConstraints.cloudExtinction.some,
      SpecPhotoConstraints.imageQuality.some,
      SpecPhotoConstraints.skyBackground.some,
      SpecPhotoConstraints.waterVapor.some,
      ElevationRangeInput.Default.some
    )

  val TwilightCalibration: ConstraintSetInput =
    ConstraintSetInput(
      TwilightConstraints.cloudExtinction.some,
      TwilightConstraints.imageQuality.some,
      TwilightConstraints.skyBackground.some,
      TwilightConstraints.waterVapor.some,
      ElevationRangeInput.Default.some
    )

  val CloudExtinctionPresetBinding: Matcher[CloudExtinction.Preset] =
    enumeratedBinding[CloudExtinction.Preset]

  val ImageQualityPresetBinding: Matcher[ImageQuality.Preset] =
    enumeratedBinding[ImageQuality.Preset]

  val SkyBackgroundBinding: Matcher[SkyBackground] =
    enumeratedBinding[SkyBackground]

  val WaterVaporBinding: Matcher[WaterVapor] =
    enumeratedBinding[WaterVapor]

  val Binding: Matcher[ConstraintSetInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ImageQualityPresetBinding.Option("imageQuality", rImage),
        CloudExtinctionPresetBinding.Option("cloudExtinction", rCloud),
        SkyBackgroundBinding.Option("skyBackground", rSky),
        WaterVaporBinding.Option("waterVapor", rWater),
        ElevationRangeInput.Binding.Option("elevationRange", rElevation)
      ) => (rCloud, rImage, rSky, rWater, rElevation).parMapN(ConstraintSetInput(_, _, _, _, _))
    }

}
