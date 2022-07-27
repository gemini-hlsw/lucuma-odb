// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.snippet
package input

import cats.syntax.parallel._
import edu.gemini.grackle.Result
import lucuma.core.enums.CloudExtinction
import lucuma.core.enums.ImageQuality
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.optics.syntax.lens._
import lucuma.odb.graphql.snippet.input.ConstraintSetInput.NominalConstraints
import lucuma.odb.graphql.util.Bindings._

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

  def edit: ConstraintSet => Result[ConstraintSet] = cs =>
    elevationRange.fold(Result(cs.elevationRange))(_.edit(cs.elevationRange)).map { er =>
      (for {
        _ <- ConstraintSet.cloudExtinction := cloudExtinction
        _ <- ConstraintSet.imageQuality    := imageQuality
        _ <- ConstraintSet.skyBackground   := skyBackground
        _ <- ConstraintSet.waterVapor      := waterVapor
        _ <- ConstraintSet.elevationRange  := er
      } yield ()).runS(cs).value
    }

}

object ConstraintSetInput {

  // TODO: figure out what the values should actually be and move to
  // TODO: ConstraintSet companion in core?
  val NominalConstraints: ConstraintSet =
    ConstraintSet(
      cloudExtinction = CloudExtinction.ThreePointZero,
      imageQuality    = ImageQuality.TwoPointZero,
      skyBackground   = SkyBackground.Bright,
      waterVapor      = WaterVapor.Wet,
      elevationRange  = ElevationRange.AirMass.Default
    )

  val CloudExtinctionBinding: Matcher[CloudExtinction] =
    enumeratedBinding[CloudExtinction]

  val ImageQualityBinding: Matcher[ImageQuality] =
    enumeratedBinding[ImageQuality]

  val SkyBackgroundBinding: Matcher[SkyBackground] =
    enumeratedBinding[SkyBackground]

  val WaterVaporBinding: Matcher[WaterVapor] =
    enumeratedBinding[WaterVapor]

  val SimpleBinding: Matcher[ConstraintSetInput] =
    ObjectFieldsBinding.rmap {
      case List(
        CloudExtinctionBinding.Option("cloudExtinction", rCloud),
        ImageQualityBinding.Option("imageQuality", rImage),
        SkyBackgroundBinding.Option("skyBackground", rSky),
        WaterVaporBinding.Option("waterVapor", rWater),
        ElevationRangeInput.SimpleBinding.Option("elevationRange", rElevation)
      ) => (rCloud, rImage, rSky, rWater, rElevation).parMapN(ConstraintSetInput(_, _, _, _, _))
    }

  val CreateBinding: Matcher[ConstraintSet] =
    SimpleBinding.rmap(_.create)

  val EditBinding: Matcher[ConstraintSet => Result[ConstraintSet]] =
    SimpleBinding.map(_.edit)

}
