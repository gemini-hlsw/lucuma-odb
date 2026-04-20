// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.eq.*
import cats.syntax.functor.*
import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.enums.Igrins2OffsetMode
import lucuma.core.enums.ObservingModeType
import lucuma.core.math.Offset
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.TelluricType
import lucuma.odb.data.Nullable
import lucuma.odb.data.Nullable.NonNull
import lucuma.odb.format.spatialOffsets.*
import lucuma.odb.graphql.binding.*

object Igrins2LongSlitInput:

  private def validateAllOnQ(
    offsets: List[Offset]
  ): Result[List[Offset]] =
    if offsets.exists(_.p =!= Offset.P.Zero) then
      Result.failure("IGRINS-2 NodAlongSlit offsets must have p = 0.")
    else
      Result(offsets)

  case class Create(
    exposureTimeMode: Option[ExposureTimeMode],
    explicitOffsetMode: Option[Igrins2OffsetMode] = None,
    explicitSaveSVCImages: Option[Boolean] = None,
    explicitOffsets: Option[List[Offset]] = None,
    telluricType: TelluricType = TelluricType.Hot
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
          BooleanBinding.Option("explicitSaveSVCImages", rSaveSVC),
          OffsetInput.Binding.List.Option("explicitOffsets", rOffsets),
          TelluricTypeBinding.Option("telluricType", rTelluricType)
        ) =>
          (rETM, rOffsetMode, rOffsets, rSaveSVC, rTelluricType).parTupled.flatMap {
            case (etm, offsetMode, offsets, saveSVC, telluricType) =>
              val create = Create(etm, offsetMode, saveSVC, offsets, telluricType.getOrElse(TelluricType.Hot))
              offsets match
                case Some(os) if offsetMode.forall(_ === Igrins2OffsetMode.NodAlongSlit) =>
                  validateAllOnQ(os).as(create)
                case _ =>
                  Result(create)
          }
      }

  case class Edit(
    exposureTimeMode: Option[ExposureTimeMode],
    explicitOffsetMode: Nullable[Igrins2OffsetMode],
    explicitSaveSVCImages: Nullable[Boolean],
    explicitOffsets: Nullable[List[Offset]],
    telluricType: Option[TelluricType]
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
        explicitOffsets.toOption,
        telluricType.getOrElse(TelluricType.Hot)
      ))

  object Edit:

    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap {
        case List(
          ExposureTimeModeInput.Binding.Option("exposureTimeMode", rETM),
          Igrins2OffsetModeBinding.Nullable("explicitOffsetMode", rOffsetMode),
          BooleanBinding.Nullable("explicitSaveSVCImages", rSaveSVC),
          OffsetInput.Binding.List.Nullable("explicitOffsets", rOffsets),
          TelluricTypeBinding.Option("telluricType", rTelluricType)
        ) =>
          (rETM, rOffsetMode, rOffsets, rSaveSVC, rTelluricType).parTupled.flatMap {
            case (etm, offsetMode, offsets, saveSVC, telluricType) =>
              val isNodAlongSlit = offsetMode match
                case NonNull(Igrins2OffsetMode.NodAlongSlit) => true
                case _                                       => false

              val edit = Edit(etm, offsetMode, saveSVC, offsets, telluricType)
              (offsets, isNodAlongSlit) match
                case (NonNull(os), true) =>
                  validateAllOnQ(os).as(edit)
                case _ =>
                  Result(edit)
          }
      }
