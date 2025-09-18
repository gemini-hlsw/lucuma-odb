// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.all.*
import lucuma.core.model.ExposureTimeMode
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.*

case class ImagingInput(
  exposureTimeMode: ExposureTimeMode,
  asterism:         List[TargetDataInput],
  constraints:      ItcConstraintsInput,
  mode:             InstrumentModesInput
)

object ImagingInput:

  val Binding: Matcher[ImagingInput] =
    ObjectFieldsBinding.rmap:
      case List(
            ExposureTimeModeInput.Binding("exposureTimeMode", exposureTimeMode),
            TargetDataInput.Binding.List("asterism", asterism),
            ItcConstraintsInput.Binding("constraints", constraints),
            InstrumentModesInput.Binding("mode", mode)
          ) =>
        (exposureTimeMode, asterism, constraints, mode)
          .parMapN(apply)
