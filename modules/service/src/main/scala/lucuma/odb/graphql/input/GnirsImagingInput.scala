// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.data.NonEmptyList
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import grackle.Result
import grackle.syntax.*
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.WavelengthOrder
import lucuma.core.math.Angle
import lucuma.odb.data.Nullable
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.binding.*
import lucuma.refined.*

object GnirsImagingInput extends ImagingFilterCheck:

  // GNIRS imaging defaults:
  // Variant = Grouped
  // Order = Increasing
  // Offsets = Spiral
  // Size = 10 (the GNIRS imaging field is far smaller than the F2/GMOS fields)
  val DefaultVariant: ImagingVariantInput =
    ImagingVariantInput.Grouped(
      WavelengthOrder.Increasing.some,
      Nullable.NonNull(TelescopeConfigGeneratorInput.SpiralInput(Angle.fromMicroarcseconds(10_000_000L), none, none)),
      none,
      Nullable.Absent
    )

  val DefaultCoadds: PosInt = 1.refined

  case class Create(
    filters:           NonEmptyList[GnirsImagingFilterInput],
    camera:            GnirsCamera,
    coadds:            PosInt                 = DefaultCoadds,
    explicitReadMode:  Option[GnirsReadMode]  = None,
    explicitWellDepth: Option[GnirsWellDepth] = None,
    variant:           ImagingVariantInput    = DefaultVariant
  ):
    def observingModeType: ObservingModeType =
      ObservingModeType.GnirsImaging

  object Create:

    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap:
        case List(
          ImagingVariantInput.Binding.Option("variant", rVariant),
          GnirsImagingFilterInput.Binding.List("filters", rFilters),
          GnirsCameraBinding.Option("camera", rCamera),
          PosIntBinding.Option("coadds", rCoadds),
          GnirsReadModeBinding.Option("explicitReadMode", rReadMode),
          GnirsWellDepthBinding.Option("explicitWellDepth", rWellDepth)
        ) =>
          (
            rVariant,
            notEmpty("GNIRS", rFilters),
            rCamera,
            rCoadds,
            rReadMode,
            rWellDepth
          ).parTupled.flatMap: (variant, filters, camera, coadds, readMode, wellDepth) =>
            camera.fold(OdbError.InvalidArgument("A 'camera' is required on creation.".some).asFailure): c =>
              Create(filters, c, coadds.getOrElse(DefaultCoadds), readMode, wellDepth, variant.getOrElse(DefaultVariant)).success

  end Create

  case class Edit(
    filters:           Option[NonEmptyList[GnirsImagingFilterInput]],
    camera:            Option[GnirsCamera],
    coadds:            Option[PosInt],
    explicitReadMode:  Nullable[GnirsReadMode],
    explicitWellDepth: Nullable[GnirsWellDepth],
    variant:           Option[ImagingVariantInput]
  ):
    val observingModeType: ObservingModeType =
      ObservingModeType.GnirsImaging

    def toCreate: Result[Create] =
      (
        Result.fromOption(filters, atLeastOne("GNIRS").asProblem),
        Result.fromOption(camera, OdbError.InvalidArgument("A 'camera' is required on creation.".some).asProblem)
      ).parMapN: (fs, c) =>
        Create(
          fs,
          c,
          coadds.getOrElse(DefaultCoadds),
          explicitReadMode.toOption,
          explicitWellDepth.toOption,
          variant.getOrElse(DefaultVariant)
        )

  object Edit:

    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap:
        case List(
          ImagingVariantInput.Binding.Option("variant", rVariant),
          GnirsImagingFilterInput.Binding.List.Option("filters", rFilters),
          GnirsCameraBinding.Option("camera", rCamera),
          PosIntBinding.Option("coadds", rCoadds),
          GnirsReadModeBinding.Nullable("explicitReadMode", rReadMode),
          GnirsWellDepthBinding.Nullable("explicitWellDepth", rWellDepth)
        ) =>
          (
            rVariant,
            notEmptyIfPresent("GNIRS", rFilters),
            rCamera,
            rCoadds,
            rReadMode,
            rWellDepth
          ).parMapN: (variant, filters, camera, coadds, readMode, wellDepth) =>
            Edit(filters, camera, coadds, readMode, wellDepth, variant)

  end Edit

end GnirsImagingInput
