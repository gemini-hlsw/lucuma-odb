// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.data.NonEmptyList
import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.Flamingos2Decker
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.Flamingos2ReadoutMode
import lucuma.core.enums.Flamingos2Reads
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.WavelengthOrder
import lucuma.core.math.Angle
import lucuma.odb.data.Nullable
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.binding.*

object Flamingos2ImagingInput extends ImagingFilterCheck:

  // F2 imaging defaults:
  // Variant = Grouped
  // Order = Increasing 
  // Offsets = Spiral
  // Size = 30
  val DefaultVariant: ImagingVariantInput =
    ImagingVariantInput.Grouped(
      WavelengthOrder.Increasing.some,
      Nullable.NonNull(TelescopeConfigGeneratorInput.SpiralInput(Angle.fromMicroarcseconds(30_000_000L), none, none)),
      none,
      Nullable.Absent
    )

  case class Create(
    filters:             NonEmptyList[Flamingos2ImagingFilterInput],
    explicitReadMode:    Option[Flamingos2ReadMode]    = None,
    explicitReads:       Option[Flamingos2Reads]       = None,
    explicitDecker:      Option[Flamingos2Decker]      = None,
    explicitReadoutMode: Option[Flamingos2ReadoutMode] = None,
    variant:             ImagingVariantInput           = DefaultVariant
  ):
    def observingModeType: ObservingModeType =
      ObservingModeType.Flamingos2Imaging

  object Create:

    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap:
        case List(
          ImagingVariantInput.Binding.Option("variant", rVariant),
          Flamingos2ImagingFilterInput.Binding.List("filters", rFilters),
          Flamingos2ReadModeBinding.Option("explicitReadMode", rReadMode),
          Flamingos2ReadsBinding.Option("explicitReads", rReads),
          Flamingos2DeckerBinding.Option("explicitDecker", rDecker),
          Flamingos2ReadoutModeBinding.Option("explicitReadoutMode", rReadoutMode)
        ) =>
          (
            rVariant,
            notEmpty("Flamingos2", rFilters),
            rReadMode,
            rReads,
            rDecker,
            rReadoutMode
          ).parMapN: (variant, filters, readMode, reads, decker, readoutMode) =>
            Create(filters, readMode, reads, decker, readoutMode, variant.getOrElse(DefaultVariant))

  end Create

  case class Edit(
    filters:             Option[NonEmptyList[Flamingos2ImagingFilterInput]],
    explicitReadMode:    Nullable[Flamingos2ReadMode],
    explicitReads:       Nullable[Flamingos2Reads],
    explicitDecker:      Nullable[Flamingos2Decker],
    explicitReadoutMode: Nullable[Flamingos2ReadoutMode],
    variant:             Option[ImagingVariantInput]
  ):
    val observingModeType: ObservingModeType =
      ObservingModeType.Flamingos2Imaging

    def toCreate: Result[Create] =
      Result.fromOption(filters, atLeastOne("Flamingos2").asProblem).map: fs =>
        Create(
          fs,
          explicitReadMode.toOption,
          explicitReads.toOption,
          explicitDecker.toOption,
          explicitReadoutMode.toOption,
          variant.getOrElse(DefaultVariant)
        )

  object Edit:

    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap:
        case List(
          ImagingVariantInput.Binding.Option("variant", rVariant),
          Flamingos2ImagingFilterInput.Binding.List.Option("filters", rFilters),
          Flamingos2ReadModeBinding.Nullable("explicitReadMode", rReadMode),
          Flamingos2ReadsBinding.Nullable("explicitReads", rReads),
          Flamingos2DeckerBinding.Nullable("explicitDecker", rDecker),
          Flamingos2ReadoutModeBinding.Nullable("explicitReadoutMode", rReadoutMode)
        ) =>
          (
            rVariant,
            notEmptyIfPresent("Flamingos2", rFilters),
            rReadMode,
            rReads,
            rDecker,
            rReadoutMode
          ).parMapN: (variant, filters, readMode, reads, decker, readoutMode) =>
            Edit(filters, readMode, reads, decker, readoutMode, variant)

  end Edit

end Flamingos2ImagingInput
