// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.data.NonEmptyList
import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.enums.ObservingModeType
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

    def exposureTimeModes: NonEmptyList[(ScorpioFilter, Option[ExposureTimeMode])] =
      NonEmptyList.of(
        ScorpioFilter.G -> g,
        ScorpioFilter.R -> r,
        ScorpioFilter.I -> i,
        ScorpioFilter.Z -> z
      )

    private val stored = explicitTelescopeConfigs.map(storedSlitTelescopeConfigs)

    val explicitSlitOffsetMode = stored.map(_.slitOffsetMode)

    val formattedTelescopeConfigs = stored.map(_.telescopeConfigs)

  object Create:

    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap:
        case List(
          ScorpioFpuBinding.NonNullable("fpu", rFpu),
          ExposureTimeModeInput.Binding.NonNullable("g", rG),
          ExposureTimeModeInput.Binding.NonNullable("r", rR),
          ExposureTimeModeInput.Binding.NonNullable("i", rI),
          ExposureTimeModeInput.Binding.NonNullable("z", rZ),
          SlitTelescopeConfigsInput.Binding.Option("explicitTelescopeConfigs", rTelescopeConfigs)
        ) =>
          val rFpuʹ = rFpu.flatMap(Result.fromOption(_, MissingFpu))
          (rFpuʹ, rG, rR, rI, rZ, rTelescopeConfigs).parMapN(Create.apply)

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

    def toCreate: Result[Create] =
      Result
        .fromOption(fpu, MissingFpu)
        .map(Create(_, g, r, i, z, explicitTelescopeConfigs.toOption))

  object Edit:

    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap:
        case List(
          ScorpioFpuBinding.NonNullable("fpu", rFpu),
          ExposureTimeModeInput.Binding.NonNullable("g", rG),
          ExposureTimeModeInput.Binding.NonNullable("r", rR),
          ExposureTimeModeInput.Binding.NonNullable("i", rI),
          ExposureTimeModeInput.Binding.NonNullable("z", rZ),
          SlitTelescopeConfigsInput.Binding.Nullable("explicitTelescopeConfigs", rTelescopeConfigs)
        ) =>
          (rFpu, rG, rR, rI, rZ, rTelescopeConfigs).parMapN(Edit.apply)
