// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.functor.*
import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.enums.Flamingos2Decker
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.Flamingos2ReadoutMode
import lucuma.core.enums.Flamingos2Reads
import lucuma.core.enums.ObservingModeType
import lucuma.core.math.Offset
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.TelluricType
import lucuma.odb.data.Nullable
import lucuma.odb.data.Nullable.NonNull
import lucuma.odb.format.spatialOffsets.*
import lucuma.odb.graphql.binding.*

object Flamingos2LongSlitInput:

  private def validateOffsets[A](offsets: List[Offset]): Result[List[Offset]] =
    if (offsets.nonEmpty && offsets.length != 4)
      Result.failure(s"Flamingos2 must have exactly 0 or 4 offsets, but ${offsets.length} were provided.")
    else
      Result(offsets)

  case class Create(
    disperser: Flamingos2Disperser,
    filter: Flamingos2Filter,
    fpu: Flamingos2Fpu,
    acquisitionExposureTimeMode: Option[ExposureTimeMode],
    scienceExposureTimeMode: Option[ExposureTimeMode],
    explicitReadMode: Option[Flamingos2ReadMode]       = None,
    explicitReads: Option[Flamingos2Reads]             = None,
    explicitDecker: Option[Flamingos2Decker]           = None,
    explicitReadoutMode: Option[Flamingos2ReadoutMode] = None,
    explicitOffsets: Option[List[Offset]]              = None,
    telluricType: TelluricType                         = TelluricType.Hot
  ):
    def observingModeType: ObservingModeType =
      ObservingModeType.Flamingos2LongSlit

    val formattedOffsets: Option[String] =
      explicitOffsets.map(OffsetsFormat.reverseGet)

  object Create:

    private val Flamingos2Data: Matcher[(
      Flamingos2Disperser,
      Flamingos2Filter,
      Flamingos2Fpu,
      Option[ExposureTimeMode],
      Option[ExposureTimeMode],
      Option[Flamingos2ReadMode],
      Option[Flamingos2Decker],
      Option[Flamingos2ReadoutMode],
      Option[Flamingos2Reads],
      Option[List[Offset]],
      Option[TelluricType]
    )] =
      ObjectFieldsBinding.rmap {
        case List(
          Flamingos2DisperserBinding("disperser", rDisperser),
          Flamingos2FilterBinding("filter", rFilter),
          Flamingos2FpuBinding("fpu", rFpu),
          ExposureTimeModeInput.Binding.Option("acquisitionExposureTimeMode", rAcqExposureTimeMode),
          ExposureTimeModeInput.Binding.Option("scienceExposureTimeMode", rSciExposureTimeMode),
          Flamingos2ReadModeBinding.Option("explicitReadMode", rReadMode),
          Flamingos2ReadsBinding.Option("explicitReads", rReads),
          Flamingos2DeckerBinding.Option("explicitDecker", rDecker),
          Flamingos2ReadoutModeBinding.Option("explicitReadoutMode", rReadoutMode),
          OffsetInput.Binding.List.Option("explicitOffsets", rOffsets),
          TelluricTypeBinding.Option("telluricType", rTelluricType)
        ) => (
          rDisperser,
          rFilter,
          rFpu,
          rAcqExposureTimeMode,
          rSciExposureTimeMode,
          rReadMode,
          rDecker,
          rReadoutMode,
          rReads,
          rOffsets,
          rTelluricType
        ).parTupled
      }

    val Binding: Matcher[Create] =
      Flamingos2Data.rmap {
        case (
          disperser,
          filter,
          fpu,
          acqExposureTimeMode,
          sciExposureTimeMode,
          explicitReadMode,
          explicitDecker,
          explicitReadoutMode,
          explicitReads,
          explicitOffsets,
          telluricType
      ) =>
        explicitOffsets match
          case Some(offsets) =>
            validateOffsets(offsets).as:
              Create(
                disperser,
                filter,
                fpu,
                acqExposureTimeMode,
                sciExposureTimeMode,
                explicitReadMode,
                explicitReads,
                explicitDecker,
                explicitReadoutMode,
                explicitOffsets,
                telluricType.getOrElse(TelluricType.Hot)
              )
          case None =>
            Result(Create(
              disperser,
              filter,
              fpu,
              acqExposureTimeMode,
              sciExposureTimeMode,
              explicitReadMode,
              explicitReads,
              explicitDecker,
              explicitReadoutMode,
              explicitOffsets,
              telluricType.getOrElse(TelluricType.Hot)
            ))
      }


  case class Edit(
    disperser: Option[Flamingos2Disperser],
    filter: Option[Flamingos2Filter],
    fpu: Option[Flamingos2Fpu],
    acquisitionExposureTimeMode: Option[ExposureTimeMode],
    scienceExposureTimeMode: Option[ExposureTimeMode],
    explicitReadMode: Nullable[Flamingos2ReadMode],
    explicitReads: Nullable[Flamingos2Reads],
    explicitDecker: Nullable[Flamingos2Decker],
    explicitReadoutMode: Nullable[Flamingos2ReadoutMode],
    explicitOffsets: Nullable[List[Offset]],
    telluricType: Option[TelluricType]
  ):

    val observingModeType: ObservingModeType =
      ObservingModeType.Flamingos2LongSlit

    val formattedOffsets: Nullable[String] =
      explicitOffsets.map(OffsetsFormat.reverseGet)

    private def required[A](oa: Option[A], itemName: String): Result[A] =
      Result.fromOption(
        oa,
        Matcher.validationProblem(s"A $itemName is required in order to create a Flamingos 2 Long Slit observing mode.")
      )

    val toCreate: Result[Create] =
      for {
        g <- required(disperser, "disperser")
        f <- required(filter, "filter")
        u <- required(fpu, "fpu")
      } yield Create(
        g,
        f,
        u,
        acquisitionExposureTimeMode,
        scienceExposureTimeMode,
        explicitReadMode.toOption,
        explicitReads.toOption,
        explicitDecker.toOption,
        explicitReadoutMode.toOption,
        explicitOffsets.toOption,
        telluricType.getOrElse(TelluricType.Hot)
      )

  object Edit:

    private val Flamingos2EditData: Matcher[(
      Option[Flamingos2Disperser],
      Option[Flamingos2Filter],
      Option[Flamingos2Fpu],
      Option[ExposureTimeMode],
      Option[ExposureTimeMode],
      Nullable[Flamingos2ReadMode],
      Nullable[Flamingos2Reads],
      Nullable[Flamingos2Decker],
      Nullable[Flamingos2ReadoutMode],
      Nullable[List[Offset]],
      Option[TelluricType]
    )] =
      ObjectFieldsBinding.rmap {
        case List(
          Flamingos2DisperserBinding.Option("disperser", rDisperser),
          Flamingos2FilterBinding.Option("filter", rFilter),
          Flamingos2FpuBinding.Option("fpu", rFpu),
          ExposureTimeModeInput.Binding.Option("acquisitionExposureTimeMode", rAcqExposureTimeMode),
          ExposureTimeModeInput.Binding.Option("scienceExposureTimeMode", rSciExposureTimeMode),
          Flamingos2ReadModeBinding.Nullable("explicitReadMode", rReadMode),
          Flamingos2ReadsBinding.Nullable("explicitReads", rReads),
          Flamingos2DeckerBinding.Nullable("explicitDecker", rDecker),
          Flamingos2ReadoutModeBinding.Nullable("explicitReadoutMode", rReadoutMode),
          OffsetInput.Binding.List.Nullable("explicitOffsets", rOffsets),
          TelluricTypeBinding.Option("telluricType", rTelluricType)
        ) => (
          rDisperser,
          rFilter,
          rFpu,
          rAcqExposureTimeMode,
          rSciExposureTimeMode,
          rReadMode,
          rReads,
          rDecker,
          rReadoutMode,
          rOffsets,
          rTelluricType
        ).parTupled
      }

    val Binding: Matcher[Edit] =
      Flamingos2EditData.rmap {
        case (
          grating,
          filter,
          fpu,
          acqExposureTimeMode,
          sciExposureTimeMode,
          explicitReadMode,
          explicitReads,
          explicitDecker,
          explicitReadoutMode,
          explicitOffsets,
          telluricType
          ) =>
          explicitOffsets match
            case NonNull(offsets) =>
              validateOffsets(offsets).as:
                Edit(
                  grating,
                  filter,
                  fpu,
                  acqExposureTimeMode,
                  sciExposureTimeMode,
                  explicitReadMode,
                  explicitReads,
                  explicitDecker,
                  explicitReadoutMode,
                  explicitOffsets,
                  telluricType
                )
            case _ =>
              Result(Edit(
                grating,
                filter,
                fpu,
                acqExposureTimeMode,
                sciExposureTimeMode,
                explicitReadMode,
                explicitReads,
                explicitDecker,
                explicitReadoutMode,
                explicitOffsets,
                telluricType
              ))
      }