// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service.requests

import cats.*
import cats.data.NonEmptyChain
import cats.derived.*
import cats.syntax.all.*
import grackle.*
import lucuma.core.enums.ExecutionEnvironment as _
import lucuma.core.model.ExposureTimeMode
import lucuma.itc.*
import lucuma.itc.input.*
import lucuma.itc.service.ItcObservingConditions
import lucuma.itc.service.ObservingMode
import lucuma.itc.service.TargetData
import lucuma.itc.service.hashes.given

case class ImagingTimeParameters(
  exposureTimeMode: ExposureTimeMode,
  imagingMode:      ObservingMode.ImagingMode,
  constraints:      ItcObservingConditions
) derives Hash

case class TargetImagingTimeRequest(
  target:     TargetData,
  parameters: ImagingTimeParameters
) extends ServiceRequest derives Hash:
  export parameters.*

case class AsterismImagingTimeRequest(
  asterism:   NonEmptyChain[TargetData],
  parameters: ImagingTimeParameters
) derives Hash:
  export parameters.*

  def toTargetRequests: NonEmptyChain[TargetImagingTimeRequest] =
    asterism.map:
      TargetImagingTimeRequest(_, parameters)

object AsterismImagingTimeRequest:
  def fromInput(input: ImagingInput): Result[AsterismImagingTimeRequest] = {
    val ImagingInput(
      exposureTimeMode,
      asterism,
      constraints,
      mode
    ) = input

    val modeResult: Result[ObservingMode.ImagingMode] =
      mode match
        case GmosNImagingInput(filter, ccdMode) =>
          Result.success:
            ObservingMode.ImagingMode.GmosNorth(filter, ccdMode)
        case GmosSImagingInput(filter, ccdMode) =>
          Result.success:
            ObservingMode.ImagingMode.GmosSouth(filter, ccdMode)
        case Flamingos2ImagingInput(filter)     =>
          Result.success:
            ObservingMode.ImagingMode.Flamingos2(filter)
        case _                                  =>
          Result.failure("Invalid imaging mode")

    (asterism.targetInputsToData, modeResult, constraints.create).parMapN:
      (asterism, mode, conditions) =>
        AsterismImagingTimeRequest(
          asterism,
          ImagingTimeParameters(exposureTimeMode, mode, conditions)
        )
  }
