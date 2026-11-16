// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.data.NonEmptyList
import cats.syntax.eq.*
import cats.syntax.functor.*
import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.ScorpioArmSet
import lucuma.core.enums.ScorpioFilter
import lucuma.core.enums.ScorpioFpu
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.odb.data.Nullable
import lucuma.odb.format.telescopeConfigs.*
import lucuma.odb.graphql.binding.*

object ScorpioLongSlitInput:

  private val MissingFpu: String =
    "A slit (fpu) must be supplied for SCORPIO Long Slit observations"

  private def checkChannelSelection(channelSelection: Option[ScorpioArmSet]): Result[Unit] =
    channelSelection.filterNot(_ === ScorpioArmSet.VisOnly).fold(Result.unit): a =>
      Result.failure(s"SCORPIO Long Slit only supports the VIS_ONLY channel selection, not ${a.tag.toUpperCase}")

  case class Create(
    fpu:                      ScorpioFpu,
    g:                        Option[ExposureTimeMode],
    r:                        Option[ExposureTimeMode],
    i:                        Option[ExposureTimeMode],
    z:                        Option[ExposureTimeMode],
    explicitTelescopeConfigs: Option[SlitTelescopeConfigs]
  ):
    def observingModeType: ObservingModeType =
      ObservingModeType.ScorpioLongSlit

    // A channel left out takes its controller partner's mode before the requirements'.
    def exposureTimeModes: NonEmptyList[(ScorpioFilter, Option[ExposureTimeMode])] =
      NonEmptyList.of(
        ScorpioFilter.G -> g.orElse(r),
        ScorpioFilter.R -> r.orElse(g),
        ScorpioFilter.I -> i.orElse(z),
        ScorpioFilter.Z -> z.orElse(i)
      )

    private val stored = explicitTelescopeConfigs.map(storedSlitTelescopeConfigs)

    val explicitSlitOffsetMode = stored.map(_.slitOffsetMode)

    val formattedTelescopeConfigs = stored.map(_.telescopeConfigs)

  object Create:

    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap:
        case List(
          ScorpioFpuBinding.NonNullable("fpu", rFpu),
          ScorpioArmSetBinding.NonNullable("channelSelection", rChannelSelection),
          ExposureTimeModeInput.Binding.NonNullable("g", rG),
          ExposureTimeModeInput.Binding.NonNullable("r", rR),
          ExposureTimeModeInput.Binding.NonNullable("i", rI),
          ExposureTimeModeInput.Binding.NonNullable("z", rZ),
          SlitTelescopeConfigsInput.Binding.Option("explicitTelescopeConfigs", rTelescopeConfigs)
        ) =>
          val rFpuʹ    = rFpu.flatMap(Result.fromOption(_, MissingFpu))
          val rChannelSelectionʹ = rChannelSelection.flatMap(checkChannelSelection)
          (rFpuʹ, rChannelSelectionʹ, rG, rR, rI, rZ, rTelescopeConfigs).parMapN: (fpu, _, g, r, i, z, tc) =>
            Create(fpu, g, r, i, z, tc)

  case class Edit(
    fpu:                      Option[ScorpioFpu],
    g:                        Option[ExposureTimeMode],
    r:                        Option[ExposureTimeMode],
    i:                        Option[ExposureTimeMode],
    z:                        Option[ExposureTimeMode],
    explicitTelescopeConfigs: Nullable[SlitTelescopeConfigs]
  ):
    def observingModeType: ObservingModeType =
      ObservingModeType.ScorpioLongSlit

    private val stored = explicitTelescopeConfigs.map(storedSlitTelescopeConfigs)

    val explicitSlitOffsetMode = stored.map(_.slitOffsetMode)

    val formattedTelescopeConfigs = stored.map(_.telescopeConfigs)

    def exposureTimeModes: List[(ScorpioFilter, ExposureTimeMode)] =
      List(
        g.tupleLeft(ScorpioFilter.G),
        r.tupleLeft(ScorpioFilter.R),
        i.tupleLeft(ScorpioFilter.I),
        z.tupleLeft(ScorpioFilter.Z)
      ).flatten

    def toCreate: Result[Create] =
      Result
        .fromOption(fpu, MissingFpu)
        .map(Create(_, g, r, i, z, explicitTelescopeConfigs.toOption))

  object Edit:

    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap:
        case List(
          ScorpioFpuBinding.NonNullable("fpu", rFpu),
          ScorpioArmSetBinding.NonNullable("channelSelection", rChannelSelection),
          ExposureTimeModeInput.Binding.NonNullable("g", rG),
          ExposureTimeModeInput.Binding.NonNullable("r", rR),
          ExposureTimeModeInput.Binding.NonNullable("i", rI),
          ExposureTimeModeInput.Binding.NonNullable("z", rZ),
          SlitTelescopeConfigsInput.Binding.Nullable("explicitTelescopeConfigs", rTelescopeConfigs)
        ) =>
          val rChannelSelectionʹ = rChannelSelection.flatMap(checkChannelSelection)
          (rFpu, rChannelSelectionʹ, rG, rR, rI, rZ, rTelescopeConfigs).parMapN: (fpu, _, g, r, i, z, tc) =>
            Edit(fpu, g, r, i, z, tc)
