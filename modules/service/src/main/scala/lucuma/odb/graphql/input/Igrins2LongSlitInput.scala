// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.TelluricType
import lucuma.odb.data.Nullable
import lucuma.odb.format.telescopeConfigs.*
import lucuma.odb.graphql.binding.*

object Igrins2LongSlitInput:

  case class Create(
    exposureTimeMode: Option[ExposureTimeMode],
    explicitSaveSVCImages: Option[Boolean] = None,
    explicitTelescopeConfigs: Option[SlitTelescopeConfigs] = None,
    telluricType: TelluricType = TelluricType.Hot
  ):
    def observingModeType: ObservingModeType =
      ObservingModeType.Igrins2LongSlit

    private val stored = explicitTelescopeConfigs.map(storedSlitTelescopeConfigs)

    val explicitSlitOffsetMode = stored.map(_.slitOffsetMode)

    val formattedTelescopeConfigs = stored.map(_.telescopeConfigs)

  object Create:

    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap {
        case List(
          ExposureTimeModeInput.Binding.Option("exposureTimeMode", rETM),
          BooleanBinding.Option("explicitSaveSVCImages", rSaveSVC),
          SlitTelescopeConfigsInput.Binding.Option("explicitTelescopeConfigs", rTelescopeConfigs),
          TelluricTypeBinding.Option("telluricType", rTelluricType)
        ) =>
          (rETM, rSaveSVC, rTelescopeConfigs, rTelluricType).parMapN { (etm, saveSVC, telescopeConfigs, telluricType) =>
            Create(etm, saveSVC, telescopeConfigs, telluricType.getOrElse(TelluricType.Hot))
          }
      }

  case class Edit(
    exposureTimeMode: Option[ExposureTimeMode],
    explicitSaveSVCImages: Nullable[Boolean],
    explicitTelescopeConfigs: Nullable[SlitTelescopeConfigs],
    telluricType: Option[TelluricType]
  ):

    val observingModeType: ObservingModeType =
      ObservingModeType.Igrins2LongSlit

    private val stored = explicitTelescopeConfigs.map(storedSlitTelescopeConfigs)

    val explicitSlitOffsetMode = stored.map(_.slitOffsetMode)

    val formattedTelescopeConfigs = stored.map(_.telescopeConfigs)

    val toCreate: Result[Create] =
      Result(Create(
        exposureTimeMode,
        explicitSaveSVCImages.toOption,
        explicitTelescopeConfigs.toOption,
        telluricType.getOrElse(TelluricType.Hot)
      ))

  object Edit:

    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap {
        case List(
          ExposureTimeModeInput.Binding.Option("exposureTimeMode", rETM),
          BooleanBinding.Nullable("explicitSaveSVCImages", rSaveSVC),
          SlitTelescopeConfigsInput.Binding.Nullable("explicitTelescopeConfigs", rTelescopeConfigs),
          TelluricTypeBinding.Option("telluricType", rTelluricType)
        ) =>
          (rETM, rSaveSVC, rTelescopeConfigs, rTelluricType).parMapN(Edit.apply)
      }
