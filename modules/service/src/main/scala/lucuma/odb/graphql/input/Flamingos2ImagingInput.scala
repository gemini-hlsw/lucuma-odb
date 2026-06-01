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
import lucuma.core.math.Offset
import lucuma.odb.data.Nullable
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.format.spatialOffsets.*
import lucuma.odb.graphql.binding.*

object Flamingos2ImagingInput extends ImagingFilterCheck:
  override val instrumentName: String = "Flamingos2"

  case class Create(
    filters:             NonEmptyList[Flamingos2ImagingFilterInput],
    explicitReadMode:    Option[Flamingos2ReadMode]    = None,
    explicitReads:       Option[Flamingos2Reads]       = None,
    explicitDecker:      Option[Flamingos2Decker]      = None,
    explicitReadoutMode: Option[Flamingos2ReadoutMode] = None,
    explicitOffsets:     Option[List[Offset]]          = None
  ):
    def observingModeType: ObservingModeType =
      ObservingModeType.Flamingos2Imaging

    val formattedOffsets: Option[String] =
      explicitOffsets.map(OffsetsFormat.reverseGet)

  object Create:

    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap:
        case List(
          Flamingos2ImagingFilterInput.Binding.List("filters", rFilters),
          Flamingos2ReadModeBinding.Option("explicitReadMode", rReadMode),
          Flamingos2ReadsBinding.Option("explicitReads", rReads),
          Flamingos2DeckerBinding.Option("explicitDecker", rDecker),
          Flamingos2ReadoutModeBinding.Option("explicitReadoutMode", rReadoutMode),
          OffsetInput.Binding.List.Option("explicitSpatialOffsets", rOffsets)
        ) =>
          (
            notEmpty(rFilters),
            rReadMode,
            rReads,
            rDecker,
            rReadoutMode,
            rOffsets
          ).parMapN: (filters, readMode, reads, decker, readoutMode, offsets) =>
            Create(filters, readMode, reads, decker, readoutMode, offsets)

  end Create

  case class Edit(
    filters:             Option[NonEmptyList[Flamingos2ImagingFilterInput]],
    explicitReadMode:    Nullable[Flamingos2ReadMode],
    explicitReads:       Nullable[Flamingos2Reads],
    explicitDecker:      Nullable[Flamingos2Decker],
    explicitReadoutMode: Nullable[Flamingos2ReadoutMode],
    explicitOffsets:     Nullable[List[Offset]]
  ):
    val observingModeType: ObservingModeType =
      ObservingModeType.Flamingos2Imaging

    val formattedOffsets: Nullable[String] =
      explicitOffsets.map(OffsetsFormat.reverseGet)

    def toCreate: Result[Create] =
      Result.fromOption(filters, atLeastOne.asProblem).map: fs =>
        Create(
          fs,
          explicitReadMode.toOption,
          explicitReads.toOption,
          explicitDecker.toOption,
          explicitReadoutMode.toOption,
          explicitOffsets.toOption
        )

  object Edit:

    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap:
        case List(
          Flamingos2ImagingFilterInput.Binding.List.Option("filters", rFilters),
          Flamingos2ReadModeBinding.Nullable("explicitReadMode", rReadMode),
          Flamingos2ReadsBinding.Nullable("explicitReads", rReads),
          Flamingos2DeckerBinding.Nullable("explicitDecker", rDecker),
          Flamingos2ReadoutModeBinding.Nullable("explicitReadoutMode", rReadoutMode),
          OffsetInput.Binding.List.Nullable("explicitSpatialOffsets", rOffsets)
        ) =>
          (
            notEmptyIfPresent(rFilters),
            rReadMode,
            rReads,
            rDecker,
            rReadoutMode,
            rOffsets
          ).parMapN: (filters, readMode, reads, decker, readoutMode, offsets) =>
            Edit(filters, readMode, reads, decker, readoutMode, offsets)

  end Edit

end Flamingos2ImagingInput
