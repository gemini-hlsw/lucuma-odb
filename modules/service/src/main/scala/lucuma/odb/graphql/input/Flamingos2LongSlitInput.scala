// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.Eq
import cats.derived.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import cats.syntax.partialOrder.*
import cats.syntax.traverse.*
import grackle.Result
import grackle.syntax.*
import lucuma.core.enums.Flamingos2Decker
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.Flamingos2ReadoutMode
import lucuma.core.enums.Flamingos2Reads
import lucuma.core.enums.ObservingModeType
import lucuma.core.math.Offset
import lucuma.core.model.Access
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.TelluricType
import lucuma.core.syntax.string.*
import lucuma.odb.data.Nullable
import lucuma.odb.data.Nullable.NonNull
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.format.spatialOffsets.*
import lucuma.odb.graphql.binding.*

object Flamingos2LongSlitInput:

  private def validateOffsets[A](offsets: List[Offset]): Result[List[Offset]] =
    if (offsets.nonEmpty && offsets.length != 4)
      Result.failure(s"Flamingos2 must have exactly 0 or 4 offsets, but ${offsets.length} were provided.")
    else
      Result(offsets)

  case class Acquisition(
    filter:           Nullable[Flamingos2Filter],
    exposureTimeMode: Option[ExposureTimeMode]
  ):
    def updatesAcquisition: Boolean =
      filter.isDefined || exposureTimeMode.isDefined

  object Acquisition:

    val Binding: Matcher[Acquisition] =
      ObjectFieldsBinding.rmap:
        case List(
          Flamingos2FilterBinding.Nullable("explicitFilter", rFilter),
          ExposureTimeModeInput.Binding.Option("exposureTimeMode", rExposureTime)
        ) => (
          rFilter.flatMap: n =>
            n.traverse: f =>
              if Flamingos2Filter.acquisition.exists(_ === f) then f.success
              else OdbError.InvalidArgument(s"'explicitFilter' must contain one of: ${Flamingos2Filter.acquisition.map(_.tag.toScreamingSnakeCase).mkString_(", ")}".some).asFailure
          ,
          rExposureTime
        ).parMapN(apply)


  case class Create(
    disperser: Flamingos2Disperser,
    filter: Flamingos2Filter,
    fpu: Flamingos2Fpu,
    exposureTimeMode: Option[ExposureTimeMode],
    explicitReadMode: Option[Flamingos2ReadMode]       = None,
    explicitReads: Option[Flamingos2Reads]             = None,
    explicitDecker: Option[Flamingos2Decker]           = None,
    explicitReadoutMode: Option[Flamingos2ReadoutMode] = None,
    explicitOffsets: Option[List[Offset]]              = None,
    telluricType: TelluricType                         = TelluricType.Hot,
    acquisition: Option[Acquisition]                   = None
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
      Option[Flamingos2ReadMode],
      Option[Flamingos2Reads],
      Option[Flamingos2Decker],
      Option[Flamingos2ReadoutMode],
      Option[List[Offset]],
      Option[TelluricType],
      Option[Acquisition]
    )] =
      ObjectFieldsBinding.rmap {
        case List(
          Flamingos2DisperserBinding("disperser", rDisperser),
          Flamingos2FilterBinding("filter", rFilter),
          Flamingos2FpuBinding("fpu", rFpu),
          ExposureTimeModeInput.Binding.Option("exposureTimeMode", rExposureTimeMode),
          Flamingos2ReadModeBinding.Option("explicitReadMode", rReadMode),
          Flamingos2ReadsBinding.Option("explicitReads", rReads),
          Flamingos2DeckerBinding.Option("explicitDecker", rDecker),
          Flamingos2ReadoutModeBinding.Option("explicitReadoutMode", rReadoutMode),
          OffsetInput.Binding.List.Option("explicitOffsets", rOffsets),
          TelluricTypeBinding.Option("telluricType", rTelluricType),
          Acquisition.Binding.Option("acquisition", rAcquisition)
        ) => (
          rDisperser,
          rFilter,
          rFpu,
          rExposureTimeMode,
          rReadMode,
          rReads,
          rDecker,
          rReadoutMode,
          rOffsets,
          rTelluricType,
          rAcquisition
        ).parTupled
      }

    val Binding: Matcher[Create] =
      Flamingos2Data.rmap:
        case (
          disperser,
          filter,
          fpu,
          exposureTimeMode,
          explicitReadMode,
          explicitReads,
          explicitDecker,
          explicitReadoutMode,
          explicitOffsets,
          telluricType,
          acquisition
        ) =>
          val create =
            Create(
              disperser,
              filter,
              fpu,
              exposureTimeMode,
              explicitReadMode,
              explicitReads,
              explicitDecker,
              explicitReadoutMode,
              explicitOffsets,
              telluricType.getOrElse(TelluricType.Hot),
              acquisition
            )

          explicitOffsets match
            case Some(offsets) => validateOffsets(offsets).as(create)
            case None          => create.success

  case class Edit(
    disperser: Option[Flamingos2Disperser],
    filter: Option[Flamingos2Filter],
    fpu: Option[Flamingos2Fpu],
    exposureTimeMode: Option[ExposureTimeMode],
    explicitReadMode: Nullable[Flamingos2ReadMode],
    explicitReads: Nullable[Flamingos2Reads],
    explicitDecker: Nullable[Flamingos2Decker],
    explicitReadoutMode: Nullable[Flamingos2ReadoutMode],
    explicitOffsets: Nullable[List[Offset]],
    telluricType: Option[TelluricType],
    acquisition: Option[Acquisition]
  ) derives Eq:

    def updatesAcquisition: Boolean =
      acquisition.exists(_.updatesAcquisition)

    def limitToPreExecution(access: Access): Boolean =
      // Staff can edit the acquisition info for ongoing observations
      access <= Access.Pi || 
        copy(acquisition = None) =!= Edit.AllUndefined

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
      for
        g <- required(disperser, "disperser")
        f <- required(filter, "filter")
        u <- required(fpu, "fpu")
      yield Create(
        g,
        f,
        u,
        exposureTimeMode,
        explicitReadMode.toOption,
        explicitReads.toOption,
        explicitDecker.toOption,
        explicitReadoutMode.toOption,
        explicitOffsets.toOption,
        telluricType.getOrElse(TelluricType.Hot),
        acquisition
      )

  object Edit:

    private val Flamingos2EditData: Matcher[(
      Option[Flamingos2Disperser],
      Option[Flamingos2Filter],
      Option[Flamingos2Fpu],
      Option[ExposureTimeMode],
      Nullable[Flamingos2ReadMode],
      Nullable[Flamingos2Reads],
      Nullable[Flamingos2Decker],
      Nullable[Flamingos2ReadoutMode],
      Nullable[List[Offset]],
      Option[TelluricType],
      Option[Acquisition]
    )] =
      ObjectFieldsBinding.rmap {
        case List(
          Flamingos2DisperserBinding.Option("disperser", rDisperser),
          Flamingos2FilterBinding.Option("filter", rFilter),
          Flamingos2FpuBinding.Option("fpu", rFpu),
          ExposureTimeModeInput.Binding.Option("exposureTimeMode", rExposureTimeMode),
          Flamingos2ReadModeBinding.Nullable("explicitReadMode", rReadMode),
          Flamingos2ReadsBinding.Nullable("explicitReads", rReads),
          Flamingos2DeckerBinding.Nullable("explicitDecker", rDecker),
          Flamingos2ReadoutModeBinding.Nullable("explicitReadoutMode", rReadoutMode),
          OffsetInput.Binding.List.Nullable("explicitOffsets", rOffsets),
          TelluricTypeBinding.Option("telluricType", rTelluricType),
          Acquisition.Binding.Option("acquisition", rAcquisition)
        ) => (
          rDisperser,
          rFilter,
          rFpu,
          rExposureTimeMode,
          rReadMode,
          rReads,
          rDecker,
          rReadoutMode,
          rOffsets,
          rTelluricType,
          rAcquisition
        ).parTupled
      }

    val Binding: Matcher[Edit] =
      Flamingos2EditData.rmap:
        case (
          grating,
          filter,
          fpu,
          exposureTimeMode,
          explicitReadMode,
          explicitReads,
          explicitDecker,
          explicitReadoutMode,
          explicitOffsets,
          telluricType,
          acquisition
        ) =>
          val edit =
            Edit(
              grating,
              filter,
              fpu,
              exposureTimeMode,
              explicitReadMode,
              explicitReads,
              explicitDecker,
              explicitReadoutMode,
              explicitOffsets,
              telluricType,
              acquisition
            )

          explicitOffsets match
            case NonNull(offsets) => validateOffsets(offsets).as(edit)
            case _                => edit.success

    private val AllUndefined: Edit = 
      Edit(None, None, None, None, Nullable.Absent, Nullable.Absent, Nullable.Absent, Nullable.Absent, Nullable.Absent, None, None)
