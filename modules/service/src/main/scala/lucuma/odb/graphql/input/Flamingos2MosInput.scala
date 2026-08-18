// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.Eq
import cats.derived.*
import cats.syntax.eq.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import grackle.Result
import lucuma.core.enums.Flamingos2CustomSlitWidth
import lucuma.core.enums.Flamingos2Decker
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2MosOffsetPreset
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.Flamingos2ReadoutMode
import lucuma.core.enums.Flamingos2Reads
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.TelluricType
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
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

  final case class Create(
    disperser:                Flamingos2Disperser,
    filter:                   Flamingos2Filter,
    customMask:               Flamingos2FpuMask.Custom,
    exposureTimeMode:         Option[ExposureTimeMode]      = None,
    explicitReadMode:         Option[Flamingos2ReadMode]    = None,
    explicitReads:            Option[Flamingos2Reads]       = None,
    explicitDecker:           Option[Flamingos2Decker]      = None,
    explicitReadoutMode:      Option[Flamingos2ReadoutMode] = None,
    offsetPreset:             Flamingos2MosOffsetPreset     = Flamingos2MosOffsetPreset.SparseField,
    explicitTelescopeConfigs: Option[SlitTelescopeConfigs]  = None,
    telluricType:             TelluricType                  = TelluricType.Hot
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
        common.offsetPreset.getOrElse(Flamingos2MosOffsetPreset.SparseField),
        common.explicitTelescopeConfigs.toOption,
        common.telluricType.getOrElse(TelluricType.Hot)
      )

  object Edit:

    final case class Common(
      exposureTimeMode:         Option[ExposureTimeMode],
      explicitReadMode:         Nullable[Flamingos2ReadMode],
      explicitReads:            Nullable[Flamingos2Reads],
      explicitDecker:           Nullable[Flamingos2Decker],
      explicitReadoutMode:      Nullable[Flamingos2ReadoutMode],
      offsetPreset:             Option[Flamingos2MosOffsetPreset],
      explicitTelescopeConfigs: Nullable[SlitTelescopeConfigs],
      telluricType:             Option[TelluricType]
    ) derives Eq:

      private val stored = explicitTelescopeConfigs.map(storedSlitTelescopeConfigs)

      val explicitSlitOffsetMode = stored.map(_.slitOffsetMode)

      val formattedTelescopeConfigs = stored.map(_.telescopeConfigs)

    val Binding: Matcher[Edit] =
      Data.rmap: (disperser, filter, customMask, common) =>
        Result(Edit(disperser, filter, customMask, common))

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
        Flamingos2MosOffsetPresetBinding.Option("offsetPreset", rOffsetPreset),
        SlitTelescopeConfigsInput.Binding.Nullable("explicitTelescopeConfigs", rTelescopeConfigs),
        TelluricTypeBinding.Option("telluricType", rTelluricType)
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
          rOffsetPreset,
          rTelescopeConfigs.flatMap(_.traverse(Flamingos2SpectroscopyInput.validateTelescopeConfigs)),
          rTelluricType
        ).parMapN(Edit.Common.apply)
      ).parTupled
