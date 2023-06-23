// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.option.*
import cats.syntax.parallel.*
import edu.gemini.grackle.Result
import lucuma.core.enums.CloudExtinction
import lucuma.core.enums.ImageQuality
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.ConstraintSetInput.NominalConstraints

final case class ConstraintSetInput(
  cloudExtinction: Option[CloudExtinction],
  imageQuality:    Option[ImageQuality],
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
      cloudExtinction = CloudExtinction.PointThree,
      imageQuality    = ImageQuality.PointEight,
      skyBackground   = SkyBackground.Bright,
      waterVapor      = WaterVapor.Wet,
      elevationRange  = ElevationRange.AirMass.Default
    )

  val Default: ConstraintSetInput =
    ConstraintSetInput(
      NominalConstraints.cloudExtinction.some,
      NominalConstraints.imageQuality.some,
      NominalConstraints.skyBackground.some,
      NominalConstraints.waterVapor.some,
      ElevationRangeInput.Default.some
    )

  val CloudExtinctionBinding: Matcher[CloudExtinction] =
    enumeratedBinding[CloudExtinction]

  val ImageQualityBinding: Matcher[ImageQuality] =
    enumeratedBinding[ImageQuality]

  val SkyBackgroundBinding: Matcher[SkyBackground] =
    enumeratedBinding[SkyBackground]

  val WaterVaporBinding: Matcher[WaterVapor] =
    enumeratedBinding[WaterVapor]

  val Binding: Matcher[ConstraintSetInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ImageQualityBinding.Option("imageQuality", rImage),
        CloudExtinctionBinding.Option("cloudExtinction", rCloud),
        SkyBackgroundBinding.Option("skyBackground", rSky),
        WaterVaporBinding.Option("waterVapor", rWater),
        ElevationRangeInput.Binding.Option("elevationRange", rElevation)
      ) => (rCloud, rImage, rSky, rWater, rElevation).parMapN(ConstraintSetInput(_, _, _, _, _))
    }

}
