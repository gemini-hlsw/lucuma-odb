// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.flatMap.*
import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ImageQuality
import lucuma.itc.CloudExtinctionInput
import lucuma.itc.ImageQualityInput
import lucuma.itc.service.ItcObservingConditions
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.binding.BigDecimalBinding
import lucuma.odb.graphql.input.*
import lucuma.odb.graphql.input.ConstraintSetInput.NominalConstraints

extension (iqi: ImageQualityInput) {
  def toArcSeconds: Result[BigDecimal] =
    iqi.value match
      case Left(preset)  => Result(preset.toImageQuality.toArcSeconds)
      case Right(arcsec) =>
        ImageQuality.fromArcSeconds(arcsec) match
          case Right(_) => Result(arcsec)
          case Left(_)  => Result.failure(s"Invalid image quality value: $arcsec")
}

extension (cei: CloudExtinctionInput) {
  def toVegaMagnitude: Result[BigDecimal] =
    cei.value match
      case Left(preset)      => Result(preset.toCloudExtinction.toVegaMagnitude)
      case Right(extinction) =>
        CloudExtinction.fromVegaMagnitude(extinction) match
          case Right(_) => Result(extinction)
          case Left(_)  => Result.failure(s"Invalid cloud extinction value: $extinction")
}

case class ItcConstraintsInput(
  cloudExtinction: CloudExtinctionInput,
  imageQuality:    ImageQualityInput,
  skyBackground:   SkyBackground,
  waterVapor:      WaterVapor,
  elevationRange:  ElevationRangeInput
) {

  def create: Result[ItcObservingConditions] =
    val erResult: Result[BigDecimal] =
      elevationRange.create.flatMap(e => Result.fromEither(ItcObservingConditions.airmass(e)))

    (imageQuality.toArcSeconds, cloudExtinction.toVegaMagnitude, erResult).parMapN: (iq, ce, er) =>
      ItcObservingConditions(iq, ce, waterVapor, skyBackground, er.toDouble)

}

object ItcConstraintsInput:
  val Default: ItcConstraintsInput =
    ItcConstraintsInput(
      CloudExtinctionInput.preset(NominalConstraints.cloudExtinction),
      ImageQualityInput.preset(NominalConstraints.imageQuality),
      NominalConstraints.skyBackground,
      NominalConstraints.waterVapor,
      ElevationRangeInput.Default
    )

  val CloudExtinctionPresetBinding: Matcher[CloudExtinction.Preset] =
    enumeratedBinding[CloudExtinction.Preset]

  val ImageQualityPresetBinding: Matcher[ImageQuality.Preset] =
    enumeratedBinding[ImageQuality.Preset]

  val SkyBackgroundBinding: Matcher[SkyBackground] =
    enumeratedBinding[SkyBackground]

  val WaterVaporBinding: Matcher[WaterVapor] =
    enumeratedBinding[WaterVapor]

  val ImageQualityInputBinding: Matcher[ImageQualityInput] =
    ObjectFieldsBinding.rmap:
      case List(
            ImageQualityPresetBinding.Option("preset", rPreset),
            BigDecimalBinding.Option("arcsec", rArcsec)
          ) =>
        (rPreset, rArcsec).parFlatMapN: (presetOpt, arcsecOpt) =>
          oneOrFail(
            presetOpt -> "preset",
            arcsecOpt -> "arcsec"
          ).map:
            case p: ImageQuality.Preset => ImageQualityInput.preset(p)
            case b: BigDecimal          => ImageQualityInput.arcsec(b)

  val CloudExtinctionInputBinding: Matcher[CloudExtinctionInput] =
    ObjectFieldsBinding.rmap:
      case List(
            CloudExtinctionPresetBinding.Option("preset", rPreset),
            BigDecimalBinding.Option("extinction", rExtinction)
          ) =>
        (rPreset, rExtinction)
          .parMapN: (presetOpt, extinctionOpt) =>
            oneOrFail(
              presetOpt     -> "preset",
              extinctionOpt -> "extinction"
            ).map:
              case p: CloudExtinction.Preset => CloudExtinctionInput.preset(p)
              case b: BigDecimal             => CloudExtinctionInput.extinction(b)
          .flatten

  val Binding: Matcher[ItcConstraintsInput] =
    ObjectFieldsBinding.rmap:
      case List(
            ImageQualityInputBinding("imageQuality", rImage),
            CloudExtinctionInputBinding("cloudExtinction", rCloud),
            SkyBackgroundBinding("skyBackground", rSky),
            WaterVaporBinding("waterVapor", rWater),
            ElevationRangeInput.Binding("elevationRange", rElevation)
          ) =>
        (rCloud, rImage, rSky, rWater, rElevation).parMapN(ItcConstraintsInput.apply)
