// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.enums.Igrins2OffsetMode
import lucuma.core.enums.ObservingModeType
import lucuma.core.math.Offset
import lucuma.core.model.ExposureTimeMode
import lucuma.odb.data.Nullable
import lucuma.odb.data.Nullable.NonNull
import lucuma.odb.format.spatialOffsets.*
import lucuma.odb.graphql.binding.*

object Igrins2LongSlitInput:

  private def validateOffsets(offsets: List[Offset]): Result[List[Offset]] =
    if (offsets.nonEmpty && offsets.length != 4)
      Result.failure(s"IGRINS-2 must have exactly 0 or 4 offsets, but ${offsets.length} were provided.")
    else
      Result(offsets)

  case class Create(
    exposureTimeMode: Option[ExposureTimeMode],
    explicitOffsetMode: Option[Igrins2OffsetMode] = None,
    explicitSaveSVCImages: Option[Boolean] = None,
    explicitOffsets: Option[List[Offset]] = None
  ):
    def observingModeType: ObservingModeType =
      ObservingModeType.Igrins2LongSlit

    val formattedOffsets: Option[String] =
      explicitOffsets.map(OffsetsFormat.reverseGet)

  object Create:

    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap {
        case List(
          ExposureTimeModeInput.Binding.Option("exposureTimeMode", rETM),
          Igrins2OffsetModeBinding.Option("explicitOffsetMode", rOffsetMode),
          OffsetInput.Binding.List.Option("explicitOffsets", rOffsets),
          BooleanBinding.Option("explicitSaveSVCImages", rSaveSVC)
        ) =>
          (rETM, rOffsetMode, rOffsets, rSaveSVC).parTupled.flatMap {
            case (etm, offsetMode, offsets, saveSVC) =>
              offsets match
                case Some(os) =>
                  validateOffsets(os).map(_ =>
                    Create(etm, offsetMode, saveSVC, offsets)
                  )
                case None =>
                  Result(Create(etm, offsetMode, saveSVC, offsets))
          }
      }

  case class Edit(
    exposureTimeMode: Option[ExposureTimeMode],
    explicitOffsetMode: Nullable[Igrins2OffsetMode],
    explicitSaveSVCImages: Nullable[Boolean],
    explicitOffsets: Nullable[List[Offset]]
  ):

    val observingModeType: ObservingModeType =
      ObservingModeType.Igrins2LongSlit

    val formattedOffsets: Nullable[String] =
      explicitOffsets.map(OffsetsFormat.reverseGet)

    val toCreate: Result[Create] =
      Result(Create(
        exposureTimeMode,
        explicitOffsetMode.toOption,
        explicitSaveSVCImages.toOption,
        explicitOffsets.toOption
      ))

  object Edit:

    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap {
        case List(
          ExposureTimeModeInput.Binding.Option("exposureTimeMode", rETM),
          Igrins2OffsetModeBinding.Nullable("explicitOffsetMode", rOffsetMode),
          OffsetInput.Binding.List.Nullable("explicitOffsets", rOffsets),
          BooleanBinding.Nullable("explicitSaveSVCImages", rSaveSVC)
        ) =>
          (rETM, rOffsetMode, rOffsets, rSaveSVC).parTupled.flatMap {
            case (etm, offsetMode, offsets, saveSVC) =>
              offsets match
                case NonNull(os) =>
                  validateOffsets(os).map(_ =>
                    Edit(etm, offsetMode, saveSVC, offsets)
                  )
                case _ =>
                  Result(Edit(etm, offsetMode, saveSVC, offsets))
          }
      }
