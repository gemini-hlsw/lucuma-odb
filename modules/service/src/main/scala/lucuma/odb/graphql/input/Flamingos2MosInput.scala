// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.Eq
import cats.derived.*
import cats.syntax.foldable.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import cats.syntax.partialOrder.*
import cats.syntax.traverse.*
import grackle.Result
import grackle.syntax.*
import lucuma.core.enums.Flamingos2CustomSlitWidth
import lucuma.core.enums.Flamingos2Decker
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.Flamingos2ReadoutMode
import lucuma.core.enums.Flamingos2Reads
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.Access
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.TelluricType
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.syntax.string.*
import lucuma.odb.data.Nullable
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.format.telescopeConfigs.*
import lucuma.odb.graphql.binding.*
import lucuma.odb.sequence.flamingos2.mos.Config

/**
 * Create and edit inputs for the Flamingos 2 MOS observing mode.
 */
object Flamingos2MosInput:

  /**
   * `Other` carries no slit width at all and it is unsupported at the moment.
   */
  private def validateCustomMask(m: Flamingos2FpuMask.Custom): Result[Flamingos2FpuMask.Custom] =
    if m.slitWidth === Flamingos2CustomSlitWidth.Other then
      OdbError.InvalidArgument(Config.OtherSlitWidthMessage.some).asFailure
    else
      Result(m)

  private def requireTimeAndCount(etm: ExposureTimeMode): Result[ExposureTimeMode] =
    etm match
      case ExposureTimeMode.TimeAndCountMode(_, _, _) => Result(etm)
      case _                                          =>
        OdbError.InvalidArgument("A Flamingos 2 MOS acquisition exposure time mode must be Time & Count.".some).asFailure

  case class Acquisition(
    filter:           Nullable[Flamingos2Filter],
    exposureTimeMode: Option[ExposureTimeMode]
  ) derives Eq:
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
          rExposureTime.flatMap(_.traverse(requireTimeAndCount))
        ).parMapN(apply)

  final case class Create(
    disperser:                Flamingos2Disperser,
    filter:                   Flamingos2Filter,
    customMask:               Flamingos2FpuMask.Custom,
    exposureTimeMode:         Option[ExposureTimeMode]      = None,
    explicitReadMode:         Option[Flamingos2ReadMode]    = None,
    explicitReads:            Option[Flamingos2Reads]       = None,
    explicitDecker:           Option[Flamingos2Decker]      = None,
    explicitReadoutMode:      Option[Flamingos2ReadoutMode] = None,
    explicitTelescopeConfigs: Option[SlitTelescopeConfigs]  = None,
    telluricType:             TelluricType                  = TelluricType.Hot,
    acquisition:              Option[Acquisition]           = None
  ):
    def observingModeType: ObservingModeType =
      ObservingModeType.Flamingos2Mos

    private val stored = explicitTelescopeConfigs.map(storedSlitTelescopeConfigs)

    val explicitSlitOffsetMode = stored.map(_.slitOffsetMode)

    val formattedTelescopeConfigs = stored.map(_.telescopeConfigs)

  object Create:

    val Binding: Matcher[Create] =
      Data.rmap: (disperser, filter, customMask, common) =>
        Edit(disperser, filter, customMask, common).toCreate

  final case class Edit(
    disperser:  Option[Flamingos2Disperser],
    filter:     Option[Flamingos2Filter],
    customMask: Option[Flamingos2FpuMask.Custom],
    common:     Edit.Common
  ) derives Eq:

    val observingModeType: ObservingModeType =
      ObservingModeType.Flamingos2Mos

    def updatesAcquisition: Boolean =
      common.updatesAcquisition

    def limitToPreExecution(access: Access): Boolean =
      // Staff can edit the acquisition info for ongoing observations
      access <= Access.Pi ||
        copy(common = common.copy(acquisition = None)) =!= Edit.AllUndefined

    private def required[A](oa: Option[A], itemName: String): Result[A] =
      Result.fromOption(
        oa,
        Matcher.validationProblem(s"A $itemName is required in order to create a Flamingos 2 MOS observing mode.")
      )

    val toCreate: Result[Create] =
      for
        d <- required(disperser, "disperser")
        f <- required(filter, "filter")
        m <- required(customMask, "customMask")
      yield Create(
        d,
        f,
        m,
        common.exposureTimeMode,
        common.explicitReadMode.toOption,
        common.explicitReads.toOption,
        common.explicitDecker.toOption,
        common.explicitReadoutMode.toOption,
        common.explicitTelescopeConfigs.toOption,
        common.telluricType.getOrElse(TelluricType.Hot),
        common.acquisition
      )

  object Edit:

    final case class Common(
      exposureTimeMode:         Option[ExposureTimeMode],
      explicitReadMode:         Nullable[Flamingos2ReadMode],
      explicitReads:            Nullable[Flamingos2Reads],
      explicitDecker:           Nullable[Flamingos2Decker],
      explicitReadoutMode:      Nullable[Flamingos2ReadoutMode],
      explicitTelescopeConfigs: Nullable[SlitTelescopeConfigs],
      telluricType:             Option[TelluricType],
      acquisition:              Option[Acquisition]
    ) derives Eq:

      def updatesAcquisition: Boolean =
        acquisition.exists(_.updatesAcquisition)

      private val stored = explicitTelescopeConfigs.map(storedSlitTelescopeConfigs)

      val explicitSlitOffsetMode = stored.map(_.slitOffsetMode)

      val formattedTelescopeConfigs = stored.map(_.telescopeConfigs)

    object Common:

      val AllUndefined: Common =
        Common(None, Nullable.Absent, Nullable.Absent, Nullable.Absent, Nullable.Absent, Nullable.Absent, None, None)

    val Binding: Matcher[Edit] =
      Data.rmap: (disperser, filter, customMask, common) =>
        Result(Edit(disperser, filter, customMask, common))

    private val AllUndefined: Edit =
      Edit(None, None, None, Common.AllUndefined)

  private val Data: Matcher[(
    Option[Flamingos2Disperser],
    Option[Flamingos2Filter],
    Option[Flamingos2FpuMask.Custom],
    Edit.Common
  )] =
    ObjectFieldsBinding.rmap:
      case List(
        Flamingos2DisperserBinding.Option("disperser", rDisperser),
        Flamingos2FilterBinding.Option("filter", rFilter),
        Flamingos2CustomMaskInput.Binding.Option("customMask", rCustomMask),
        ExposureTimeModeInput.Binding.Option("exposureTimeMode", rExposureTimeMode),
        Flamingos2ReadModeBinding.Nullable("explicitReadMode", rReadMode),
        Flamingos2ReadsBinding.Nullable("explicitReads", rReads),
        Flamingos2DeckerBinding.Nullable("explicitDecker", rDecker),
        Flamingos2ReadoutModeBinding.Nullable("explicitReadoutMode", rReadoutMode),
        SlitTelescopeConfigsInput.Binding.Nullable("explicitTelescopeConfigs", rTelescopeConfigs),
        TelluricTypeBinding.Option("telluricType", rTelluricType),
        Acquisition.Binding.Option("acquisition", rAcquisition)
      ) => (
        rDisperser,
        rFilter,
        rCustomMask.flatMap(_.traverse(validateCustomMask)),
        (
          rExposureTimeMode,
          rReadMode,
          rReads,
          rDecker,
          rReadoutMode,
          rTelescopeConfigs.flatMap(_.traverse(Flamingos2SpectroscopyInput.validateTelescopeConfigs)),
          rTelluricType,
          rAcquisition
        ).parMapN(Edit.Common.apply)
      ).parTupled
