// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.enums.Igrins2OffsetMode
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.ExposureTimeMode
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

object Igrins2LongSlitInput:

  case class Create(
    exposureTimeMode: Option[ExposureTimeMode],
    explicitOffsetMode: Option[Igrins2OffsetMode] = None,
    explicitSaveSVCImages: Option[Boolean] = None
  ):
    def observingModeType: ObservingModeType =
      ObservingModeType.Igrins2LongSlit

  object Create:

    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap {
        case List(
          ExposureTimeModeInput.Binding.Option("exposureTimeMode", rETM),
          Igrins2OffsetModeBinding.Option("explicitOffsetMode", rOffsetMode),
          BooleanBinding.Option("explicitSaveSVCImages", rSaveSVC)
        ) =>
          (rETM, rOffsetMode, rSaveSVC).parTupled.map(Create.apply)
      }

  case class Edit(
    exposureTimeMode: Option[ExposureTimeMode],
    explicitOffsetMode: Nullable[Igrins2OffsetMode],
    explicitSaveSVCImages: Nullable[Boolean]
  ):

    val observingModeType: ObservingModeType =
      ObservingModeType.Igrins2LongSlit

    val toCreate: Result[Create] =
      Result(Create(
        exposureTimeMode,
        explicitOffsetMode.toOption,
        explicitSaveSVCImages.toOption
      ))

  object Edit:

    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap {
        case List(
          ExposureTimeModeInput.Binding.Option("exposureTimeMode", rETM),
          Igrins2OffsetModeBinding.Nullable("explicitOffsetMode", rOffsetMode),
          BooleanBinding.Nullable("explicitSaveSVCImages", rSaveSVC)
        ) =>
          (rETM, rOffsetMode, rSaveSVC).parTupled.map(Edit.apply)
      }
